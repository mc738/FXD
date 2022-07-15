namespace FXD.CodeDocuments.FSharp

module Templating =

     open System.IO
     open System.Text.RegularExpressions
     open FXD.CodeDocuments.FSharp
     open Fluff.Core    
     open XmlDocExtractor
     open Documentation
     
     let handleXmlDoc doc =
        match doc with
        | Some xdoc ->
            let summary =
                match xdoc.Summary with
                | Some s -> Some s
                | None -> None

            let returns =
                match xdoc.Returns with
                | Some s -> Some s
                | None -> None

            let examples = xdoc.Examples

            summary, examples, returns
        | None -> None, [], None
     
     let createMethodData (m: MethodDocument) =
        let (summary, examples, returns) =
            handleXmlDoc m.XmlDocument

        [ "id", Mustache.Value.Scalar m.Id
          "name", Mustache.Value.Scalar m.Name
          "signature", Mustache.Value.Scalar m.Signature
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "parameters",
          m.Parameters
          |> List.map (fun p ->
              [ "name", Mustache.Value.Scalar p.Name
                "type", Mustache.Value.Scalar p.Type
                "document", Mustache.Value.Scalar(p.Document |> Option.defaultValue "") ]
              |> Map.ofList
              |> Mustache.Value.Object)
          |> Mustache.Value.Array
          "examples",
          examples
          |> List.map Mustache.Value.Scalar
          |> Mustache.Value.Array
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "") ]
        |> Map.ofList
        |> Mustache.Value.Object

     let createPropertyData (p: PropertyDocument) =
        let (summary, examples, returns) =
            handleXmlDoc p.XmlDocument

        [ "id", Mustache.Value.Scalar p.Id
          "name", Mustache.Value.Scalar p.Name
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "") ]
        |> Map.ofList
        |> Mustache.Value.Object

     let createMethods (ms: MethodDocument list) =
        match ms.IsEmpty with
        | true -> None
        | false ->
            [ "items", ms |> List.map createMethodData |> Mustache.Array ]
            |> Map.ofList
            |> Mustache.Value.Object
            |> Some

     let createProperties (ms: PropertyDocument list) =
        match ms.IsEmpty with
        | true -> None
        | false ->
            [ "items",
              ms
              |> List.map createPropertyData
              |> Mustache.Array ]
            |> Map.ofList
            |> Mustache.Value.Object
            |> Some

     let createClassData (c: ClassDocument) =
        let (summary, examples, returns) =
            handleXmlDoc c.XmlDocument

        [ "id", Mustache.Value.Scalar c.Id
          "name", Mustache.Value.Scalar c.DisplayName
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          match createMethods c.Methods with
          | Some m -> "methods", m
          | None -> ()
          match createProperties c.Properties with
          | Some p -> "properties", p
          | None -> () ]
        |> Map.ofList
        |> Mustache.Value.Object

     let createRecordData (r: RecordDocument) =
        let (summary, examples, returns) =
            handleXmlDoc r.XmlDocument

        [ "id", Mustache.Value.Scalar r.Id
          "name", Mustache.Value.Scalar r.DisplayName
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "fields",
          r.Fields
          |> List.map (fun f ->
              [ "name", Mustache.Value.Scalar f.Name
                "type", Mustache.Value.Scalar f.Type ]
              |> Map.ofList
              |> Mustache.Value.Object)
          |> Mustache.Value.Array
          match createMethods r.Methods with
          | Some m -> "methods", m
          | None -> ()
          match createProperties r.Properties with
          | Some p -> "properties", p
          | None -> () ]
        |> Map.ofList
        |> Mustache.Value.Object

     let createUnionData (u: UnionDocument) =
        let (summary, examples, returns) =
            handleXmlDoc u.XmlDocument

        [ "id", Mustache.Value.Scalar u.Id
          "name", Mustache.Value.Scalar u.DisplayName
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "union_cases",
          u.Members
          |> List.map (fun uc ->
              [ "name", Mustache.Value.Scalar uc.Name
                "type", Mustache.Value.Scalar uc.Type ]
              |> Map.ofList
              |> Mustache.Value.Object)
          |> Mustache.Value.Array
          match createMethods u.Methods with
          | Some m -> "methods", m
          | None -> ()
          match createProperties u.Properties with
          | Some p -> "properties", p
          | None -> () ]
        |> Map.ofList
        |> Mustache.Value.Object

     let createFunctionParameterData (p: FunctionParameter) =
        [ "name", Mustache.Value.Scalar p.Name
          "type", Mustache.Value.Scalar p.Type
          "document", Mustache.Value.Scalar(p.Document |> Option.defaultValue "") ]
        |> Map.ofList
        |> Mustache.Value.Object

     let createFunctionData (f: FunctionDocument) =
        let (summary, examples, returns) =
            handleXmlDoc f.XmlDocument

        [ "id", Mustache.Value.Scalar f.Id
          "name", Mustache.Value.Scalar f.DisplayName
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "parameters",
          f.Parameters
          |> List.map createFunctionParameterData
          |> Mustache.Value.Array
          "examples",
          examples
          |> List.map Mustache.Value.Scalar
          |> Mustache.Value.Array
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "") ]
        |> Map.ofList
        |> Mustache.Value.Object

     let createModuleData
        (indexHtml: string)
        (title: string)
        (additionValues: (string * Mustache.Value) list)
        (m: ModuleDocument)
        =
        let classes =
            m.GetClasses() |> List.map createClassData //|> Mustache.Value.Array

        let records =
            m.GetRecords() |> List.map createRecordData

        let unions =
            m.GetUnions() |> List.map createUnionData

        let functions =
            m.GetFunctions() |> List.map createFunctionData

        [ "index", Mustache.Value.Scalar indexHtml
          "doc_name", Mustache.Value.Scalar $"Peeps API - {m.DisplayName}"
          "name", Mustache.Value.Scalar m.DisplayName
          "title", Mustache.Value.Scalar title
          "namespace", Mustache.Value.Scalar m.Namespace
          // TODO add this outside
          if classes |> List.isEmpty |> not then
              "class_collection",
              [ "classes", Mustache.Value.Array classes ]
              |> Map.ofList
              |> Mustache.Value.Object

          if records |> List.isEmpty |> not then
              "record_collection",
              [ "records", Mustache.Value.Array records ]
              |> Map.ofList
              |> Mustache.Value.Object

          if unions |> List.isEmpty |> not then
              "union_collection",
              [ "unions", Mustache.Value.Array unions ]
              |> Map.ofList
              |> Mustache.Value.Object

          if functions.IsEmpty |> not then
              "function_collection",
              [ "functions", Mustache.Value.Array functions ]
              |> Map.ofList
              |> Mustache.Value.Object

          ]
        @ additionValues
        |> Map.ofList

     let generateIndex
        (indexEntries: Map<string, string>)
        (sectionId: string)
        (name: string)
        (regexIgnore: string)
        (members: Member list)
        =
        indexEntries
        |> Map.toList
        |> List.map (fun (id, v) ->
            match sectionId = id with
            | true ->
                let contents =
                    members
                    |> List.fold
                        (fun (a, c, r, u, f, m) cm ->
                            match cm.MatchName(regexIgnore), cm with
                            | false, Member.Abbreviation _ -> a @ [ cm ], c, r, u, f, m
                            | false, Member.Class _ -> a, c @ [ cm ], r, u, f, m
                            | false, Member.Record _ -> a, c, r @ [ cm ], u, f, m
                            | false, Member.Union _ -> a, c, r, u @ [ cm ], f, m
                            | false, Member.Function _ -> a, c, r, u, f @ [ cm ], m
                            | false, Member.Module _ -> a, c, r, u, f, m @ [ cm ]
                            | true, _ -> a, c, r, u, f, m
                            | _, Member.Namespace _ -> a, c, r, u, f, m)
                        ([], [], [], [], [], [])
                    |> fun (a, c, r, u, f, m) -> [ a; c; r; u; f; m ] |> List.concat
                    |> List.map (fun m -> $"""<a href="#{m.GetId()}">{m.GetDisplayName()}</a>""")
                    |> String.concat ""

                $"""<li class="open"><h3>{name}</h3>{contents}</li>"""
            | false -> v)

        |> String.concat ""
        |> fun indexes -> $"""<div class="index-section"><h2>API reference</h2><ul>{indexes}</ul></div>"""

     let generateModulePage
        (template: Mustache.Token list)
        (title: string)
        (indexHtml: string)
        (additionValues: (string * Mustache.Value) list)
        (m: ModuleDocument)
        =
        
        ({ Values = createModuleData indexHtml title additionValues m
           Partials = Map.empty }: Mustache.Data)
        |> fun d -> Mustache.replace d true template
        //|> fun r -> File.WriteAllText(Path.Combine(savePath, $"{m.Id}.html"), r)

     let createIndex (regexIgnore: string) (modules: ModuleDocument list) =
        modules
        |> List.map (fun m ->
            match Regex.IsMatch(m.FullName, regexIgnore) with
            | true -> None
            | false -> Some(m.Id, $"""<li><a href="./{m.Id}.html">{m.DisplayName}</a></li>"""))
        |> List.choose id
        |> Map.ofList

     (*
     let createModules
        (template: Mustache.Token list)
        (savePath: string)
        (title: string)
        (regexIgnore: string)
        (additionalValues: (string * Mustache.Value) list)
        (members: Member list)
        =
        // TODO Handle top level non module.
        let (modules, topLevel) =
            members
            |> List.fold
                (fun (acc, topLevel) m ->
                    match m with
                    | Member.Module mm -> acc @ [ mm ], topLevel
                    | _ -> acc, topLevel @ [])
                ([], [])
        //|> fun (acc, topLevel) ->
        //    ModuleDocument.Create()

        let indexes =
            createIndex regexIgnore modules

        modules
        |> List.iter (generateModulePage template savePath title indexes regexIgnore additionalValues)
     *)