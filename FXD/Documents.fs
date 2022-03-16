namespace FXD

open System
open System.Configuration
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open FDOM.Core.Parsing
open FDOM.Rendering
open FXD
open FXD.Documentation
open FXD.XmlDocExtractor
open Fluff.Core

[<RequireQualifiedAccess>]
module Templating =

    let slugifyName (name: string) =
        name
        |> Seq.fold
            (fun acc c ->
                match c with
                | _ when Char.IsLetterOrDigit c -> acc @ [ Char.ToLower c ]
                | _ when c = ' ' -> acc @ [ '_' ]
                | _ when c = '-' -> acc @ [ c ]
                | _ -> acc)
            []
        |> fun c -> String.Join("", c)

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

    let createClassValues (extraValues: (string * Mustache.Value) list) (cd: ClassDocument) =
        let (summary, examples, returns) = handleXmlDoc cd.XmlDocument

        [ "id", Mustache.Value.Scalar cd.Id
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "name", Mustache.Value.Scalar cd.DisplayName
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "properties",
          Mustache.Value.Array(
              cd.Properties
              |> List.map
                  (fun p ->
                      let (pSummary, pExamples, pReturns) = handleXmlDoc p.XmlDocument

                      [ "id", Mustache.Value.Scalar p.Id
                        "className", Mustache.Scalar cd.DisplayName
                        "name", Mustache.Value.Scalar p.Name
                        "type", Mustache.Value.Scalar p.Type
                        "summary", Mustache.Value.Scalar(pSummary |> Option.defaultValue "")
                        "returns", Mustache.Value.Scalar(pReturns |> Option.defaultValue "")
                        "examples", Mustache.Value.Array(pExamples |> List.map Mustache.Value.Scalar) ]
                      |> Map.ofList
                      |> Mustache.Object)
          )
          "methods",
          Mustache.Value.Array(
              cd.Methods
              |> List.map
                  (fun m ->
                      let (mSummary, mExamples, mReturns) = handleXmlDoc m.XmlDocument

                      [ "id", Mustache.Value.Scalar m.Id
                        "className", Mustache.Scalar ""
                        "name", Mustache.Value.Scalar m.Name
                        "signature", Mustache.Value.Scalar m.Signature
                        "parameters",
                        m.Parameters
                        |> List.map
                            (fun p ->
                                [ "name", Mustache.Value.Scalar p.Name
                                  "document", Mustache.Value.Scalar(p.Document |> Option.defaultValue "")
                                  "type", Mustache.Value.Scalar p.Type ]
                                |> Map.ofList
                                |> Mustache.Value.Object)
                        |> Mustache.Value.Array
                        "summary", Mustache.Value.Scalar(mSummary |> Option.defaultValue "")
                        "returns", Mustache.Value.Scalar(mReturns |> Option.defaultValue "")
                        "examples", Mustache.Value.Array(mExamples |> List.map Mustache.Value.Scalar) ]
                      |> Map.ofList
                      |> Mustache.Object)
          ) ]
        @ extraValues
        |> Map.ofList
        
    let createClassData (cd: ClassDocument) =
        createClassValues [] cd |> Mustache.Value.Object

    let createFunctionData (fd: FunctionDocument) =
        let (summary, examples, returns) = handleXmlDoc fd.XmlDocument

        [ "id", Mustache.Value.Scalar fd.Id
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "signature", Mustache.Value.Scalar fd.Signature
          "name", Mustache.Value.Scalar fd.DisplayName
          "parameters",
          Mustache.Value.Array(
              fd.Parameters
              |> List.map
                  (fun p ->
                      [ "id", Mustache.Value.Scalar p.Id
                        "name", Mustache.Value.Scalar p.Name
                        "document", Mustache.Value.Scalar(p.Document |> Option.defaultValue "")
                        "type", Mustache.Value.Scalar p.Type ]
                      |> Map.ofList
                      |> Mustache.Object)
          ) ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createRecordData (rd: RecordDocument) =
        let (summary, examples, returns) = handleXmlDoc rd.XmlDocument

        [ "id", Mustache.Value.Scalar rd.Id
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "name", Mustache.Value.Scalar rd.DisplayName
          "fields",
          Mustache.Value.Array(
              rd.Fields
              |> List.map
                  (fun f ->
                      let (fSummary, fExamples, fReturns) = handleXmlDoc f.XmlDocument

                      [ "id", Mustache.Value.Scalar f.Id
                        "summary", Mustache.Value.Scalar(fSummary |> Option.defaultValue "")
                        "returns", Mustache.Value.Scalar(fReturns |> Option.defaultValue "")
                        "examples", Mustache.Value.Array(fExamples |> List.map Mustache.Value.Scalar)
                        "name", Mustache.Value.Scalar f.Name
                        "type", Mustache.Value.Scalar f.Type ]
                      |> Map.ofList
                      |> Mustache.Object)
          ) ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createUnionData (ud: UnionDocument) =
        let (summary, examples, returns) = handleXmlDoc ud.XmlDocument

        [ "id", Mustache.Value.Scalar ud.Id
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "name", Mustache.Value.Scalar ud.DisplayName
          "members",
          Mustache.Value.Array(
              ud.Members
              |> List.map
                  (fun f ->
                      let (fSummary, fExamples, fReturns) = handleXmlDoc f.XmlDocument

                      [ "id", Mustache.Value.Scalar f.Id
                        "summary", Mustache.Value.Scalar(fSummary |> Option.defaultValue "")
                        "returns", Mustache.Value.Scalar(fReturns |> Option.defaultValue "")
                        "examples", Mustache.Value.Array(fExamples |> List.map Mustache.Value.Scalar)
                        "name", Mustache.Value.Scalar f.Name
                        "type", Mustache.Value.Scalar f.Type ]
                      |> Map.ofList
                      |> Mustache.Object)
          ) ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createMemberData (m: Member) =
        match m with
        | Member.Class cd -> createClassData cd
        | Member.Function fd -> createFunctionData fd
        | Member.Module md -> failwith ""
        | Member.Namespace nd -> failwith ""
        | Member.Record rd -> createRecordData rd
        | Member.Union ud -> createUnionData ud

    let createData (members: Member list) =
        members
        |> List.fold
            (fun (cAcc, fAcc, mAcc, nAcc, rAcc, uAcc) m ->
                match m with
                | Member.Class cd -> cAcc @ [ createClassData cd ], fAcc, mAcc, nAcc, rAcc, uAcc
                | Member.Function fd -> cAcc, fAcc @ [ createFunctionData fd ], mAcc, nAcc, rAcc, uAcc
                | Member.Module md -> cAcc, fAcc, mAcc, nAcc, rAcc, uAcc
                | Member.Namespace nd -> cAcc, fAcc, mAcc, nAcc, rAcc, uAcc
                | Member.Record rd -> cAcc, fAcc, mAcc, nAcc, rAcc @ [ createRecordData rd ], uAcc
                | Member.Union ud -> cAcc, fAcc, mAcc, nAcc, rAcc, uAcc @ [ createUnionData ud ])
            ([], [], [], [], [], [])
        |> fun (cAcc, fAcc, mAcc, nAcc, rAcc, uAcc) ->
            ({ Values =
                   [ "classes", Mustache.Value.Array cAcc
                     "functions", Mustache.Value.Array fAcc
                     "modules", Mustache.Value.Array mAcc
                     "namespaces", Mustache.Value.Array nAcc
                     "records", Mustache.Value.Array rAcc
                     "unions", Mustache.Value.Array uAcc ]
                   |> Map.ofList
               Partials = Map.empty }: Mustache.Data)

[<RequireQualifiedAccess>]
module Pipeline =

    [<CLIMutable>]
    type Configuration =
        { [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("args")>]
          Args: Arg seq
          [<JsonPropertyName("metadata")>]
          Metadata: MetadataItem seq
          [<JsonPropertyName("directories")>]
          Directories: string seq
          [<JsonPropertyName("pages")>]
          Pages: PageConfiguration seq
          [<JsonPropertyName("codeDocuments")>]
          CodeDocuments: CodeDocumentConfiguration
          [<JsonPropertyName("resources")>]
          Resources: ResourceConfiguration seq }

    and [<CLIMutable>] Arg =
        { [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("default")>]
          Default: string
          [<JsonPropertyName("required")>]
          Required: bool }

    and [<CLIMutable>] PageConfiguration =
        { [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("template")>]
          Template: string
          [<JsonPropertyName("source")>]
          Source: string
          [<JsonPropertyName("title")>]
          Title: string
          [<JsonPropertyName("titleSlug")>]
          TitleSlug: string
          [<JsonPropertyName("output")>]
          Output: string
          [<JsonPropertyName("metadata")>]
          Metadata: MetadataItem seq }

    and [<CLIMutable>] CodeDocumentConfiguration =
        { [<JsonPropertyName("namespaceTemplate")>]
          NamespaceTemplate: string
          [<JsonPropertyName("classTemplate")>]
          ClassTemplate: string
          [<JsonPropertyName("recordTemplate")>]
          RecordTemplate: string
          [<JsonPropertyName("unionTemplate")>]
          UnionTemplate: string
          [<JsonPropertyName("functionTemplate")>]
          FunctionTemplate: string
          [<JsonPropertyName("moduleTemplate")>]
          ModuleTemplate: string
          [<JsonPropertyName("directoryName")>]
          DirectoryName: string
          [<JsonPropertyName("sources")>]
          Sources: CodeDocumentSource seq }

    and [<CLIMutable>] CodeDocumentSource =
        { [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("source")>]
          Source: string
          [<JsonPropertyName("metadata")>]
          Metadata: MetadataItem seq }

    and [<CLIMutable>] ResourceConfiguration =
        { [<JsonPropertyName("path")>]
          Path: string
          [<JsonPropertyName("outputPath")>]
          outputPath: string }

    and [<CLIMutable>] MetadataItem =
        { [<JsonPropertyName("key")>]
          Key: string
          [<JsonPropertyName("value")>]
          Value: string }

    type Context =
        { Name: string
          Templates: TemplateCache
          Actions: Action list
          RootPath: string
          OutputRoot: string }

    and Action =
        | GenerateCodeDocument of GenerateCodeDocumentAction
        | GeneratePage of GeneratePageAction
        | CreateDirectory of CreateDirectoryAction
        | CopyFile of CopyFileAction

    and GenerateCodeDocumentAction =
        { Source: string
          NamespaceTemplate: string
          ClassTemplate: string
          RecordTemplate: string
          UnionTemplate: string
          FunctionTemplate: string
          ModuleTemplate: string
          DirectoryName: string
          OutputDirectoryPath: string
          Metadata: Map<string, string> }

    and GeneratePageAction =
        { Source: string
          Template: string
          Output: string
          Title: string
          TitleSlug: string
          Metadata: Map<string, string> }

    and CreateDirectoryAction = { Path: string }

    and CopyFileAction = { From: string; To: string }

    and TemplateCache =
        { Values: Map<string, Mustache.Token list> }

        static member Empty = { Values = Map.empty }

        /// Will check for a template, if not found. It will attempt to load and parse the template.
        /// In this
        member tc.LoadAndAdd(path: string) =
            match tc.Values.TryFind path with
            | Some _ -> tc
            | None ->
                File.ReadAllText path
                |> Mustache.parse
                |> fun r ->
                    { tc with
                          Values = tc.Values.Add(path, r) }

        member tc.LoadAndAddMultiple(paths: string list) =
            paths
            |> List.fold
                (fun (tm: Map<string, Mustache.Token list>) p ->
                    match tc.Values.TryFind p with
                    | Some _ -> tm
                    | None ->
                        File.ReadAllText <| p
                        |> Mustache.parse
                        |> fun r -> tm.Add(p, r))
                tc.Values
            |> fun r -> { tc with Values = r }

    let passThru (members: Member list) (errors: Linter.LintingError list) =
        Console.ForegroundColor <- ConsoleColor.Magenta

        errors
        |> List.map (fun e -> printfn $"{e}")
        |> ignore

        Console.ResetColor()
        members

    module Internal =

        let generatePage
            (templates: TemplateCache)
            (rootPath: string)
            (outputPath: string)
            (action: GeneratePageAction)
            =
            match templates.Values.TryFind action.Template with
            | Some template ->
                let values =
                    ({ Values =
                           [ "title", Mustache.Value.Scalar action.Title
                             "titleSlug", Mustache.Value.Scalar action.TitleSlug
                             "now", Mustache.Value.Scalar(DateTime.Now.ToString("dd MMMM yyyy HH:mm:ss")) ]
                           @ (action.Metadata
                              |> Map.toList
                              |> List.map (fun (k, v) -> k, Mustache.Value.Scalar v))
                           |> Map.ofList
                       Partials = Map.empty }: Mustache.Data)

                let blocks =
                    Parser
                        .ParseLines(
                            File.ReadAllLines(Path.Combine(rootPath, action.Source))
                            |> List.ofArray
                        )
                        .CreateBlockContent()

                let doc: FDOM.Core.Common.DOM.Document =
                    { Style = FDOM.Core.Common.DOM.Style.Default
                      Name = action.Title
                      Title = None
                      Sections =
                          [ { Style = FDOM.Core.Common.DOM.Style.Default
                              Title = None
                              Name = "Section 1"
                              Content = blocks } ]
                      Resources =
                          [ { Name = "main_css"
                              Path = "/home/max/Data/FDOM_Tests/css/style.css"
                              VirtualPath = "css/style.css"
                              Type = "stylesheet" } ] }

                Html.renderFromParsedTemplate template values [] [] doc
                |> fun r -> File.WriteAllText(outputPath, r)
                |> Ok
            | None -> Error $"Template `{action.Template}` not found."

        let substituteValue (args: Map<string, string>) (value: string) =
            match value.StartsWith('@') with
            | true -> args.TryFind value.[1..] |> Option.defaultValue ""
            | false -> value

        let expandPath (root: string) (path: string) = Path.Combine(root, path)

        let createMetadata (args: Map<string, string>) (globalMetadata: Map<string, string>) (items: MetadataItem seq) =
            items
            |> List.ofSeq
            |> List.map (fun i -> substituteValue args i.Key, substituteValue args i.Value)
            |> Map.ofList
            |> Map.fold (fun (acc: Map<string, string>) k v -> acc.Add(k, v)) globalMetadata

        let createDirectoryActions (paths: string list) =
            paths
            |> List.map (fun d -> CreateDirectory { Path = d })

        let createPageActions (args: Map<string, string>) (globalMetadata: Map<string, string>) (rootPath: string) (cfg: Configuration) (acc: Action list) =
            cfg.Pages
            |> List.ofSeq
            |> List.fold
                (fun (a, tc: TemplateCache) p ->
                    let templatePath =
                        substituteValue args p.Template
                        |> expandPath rootPath

                    let gp =
                        GeneratePage
                            { Source = substituteValue args p.Source
                              Template = templatePath
                              Output = substituteValue args p.Output
                              Title = substituteValue args p.Title
                              TitleSlug = substituteValue args p.Template
                              Metadata = createMetadata args globalMetadata p.Metadata }

                    a @ [ gp ], tc.LoadAndAdd templatePath)
                ([], TemplateCache.Empty)
            |> fun (a, t) -> acc @ a, t

        let createCodeDocumentActions
            (args: Map<string, string>)
            (globalMetadata: Map<string, string>)
            (rootPath: string)
            (cfg: Configuration)
            ((acc, tc): Action list * TemplateCache)
            =

            let ntc =
                [ cfg.CodeDocuments.ClassTemplate
                  |> substituteValue args
                  |> expandPath rootPath
                  cfg.CodeDocuments.FunctionTemplate
                  |> substituteValue args
                  |> expandPath rootPath
                  cfg.CodeDocuments.ModuleTemplate
                  |> substituteValue args
                  |> expandPath rootPath
                  cfg.CodeDocuments.NamespaceTemplate
                  |> substituteValue args
                  |> expandPath rootPath
                  cfg.CodeDocuments.RecordTemplate
                  |> substituteValue args
                  |> expandPath rootPath
                  cfg.CodeDocuments.UnionTemplate
                  |> substituteValue args
                  |> expandPath rootPath ]
                |> fun p -> tc.LoadAndAddMultiple(p)

            let na =
                cfg.CodeDocuments.Sources
                |> List.ofSeq
                |> List.map
                    (fun cs ->
                        let dirName =
                            substituteValue args cfg.CodeDocuments.DirectoryName

                        // TODO Index page (i.e. all sources)

                        [ CreateDirectory { Path = dirName }
                          GenerateCodeDocument
                              { Source = substituteValue args cs.Source
                                ClassTemplate =
                                    substituteValue args cfg.CodeDocuments.ClassTemplate
                                    |> substituteValue args
                                    |> expandPath rootPath
                                NamespaceTemplate =
                                    substituteValue args cfg.CodeDocuments.NamespaceTemplate
                                    |> substituteValue args
                                    |> expandPath rootPath
                                RecordTemplate =
                                    substituteValue args cfg.CodeDocuments.RecordTemplate
                                    |> substituteValue args
                                    |> expandPath rootPath
                                UnionTemplate =
                                    substituteValue args cfg.CodeDocuments.UnionTemplate
                                    |> substituteValue args
                                    |> expandPath rootPath
                                FunctionTemplate =
                                    substituteValue args cfg.CodeDocuments.FunctionTemplate
                                    |> substituteValue args
                                    |> expandPath rootPath
                                ModuleTemplate =
                                    substituteValue args cfg.CodeDocuments.ModuleTemplate
                                    |> substituteValue args
                                    |> expandPath rootPath
                                DirectoryName = dirName
                                OutputDirectoryPath = ""
                                Metadata = createMetadata args globalMetadata cs.Metadata } ])
                |> List.concat

            acc @ na, ntc


        let createResourceActions (cfg: Configuration) =
            cfg.Resources
            |> List.ofSeq
            |> List.map (fun rc -> CopyFile { From = rc.Path; To = rc.outputPath })


        let createContext (path: string) (outputRoot: string) (args: Map<string, string>) (cfg: Configuration) =
            let argsV =
                cfg.Args
                |> List.ofSeq
                |> List.fold
                    (fun (acc: Map<string, string>) a ->
                        match args.TryFind a.Name with
                        | Some v -> acc.Add(a.Name, v)
                        | None -> acc.Add(a.Name, a.Default))
                    Map.empty

            let globalMetadata = cfg.Metadata |> createMetadata argsV Map.empty

            let (actions, templates) =
                cfg.Directories
                |> List.ofSeq
                |> createDirectoryActions
                |> createPageActions argsV globalMetadata path cfg
                |> createCodeDocumentActions globalMetadata argsV path cfg

            ({ Name = cfg.Name
               Templates = templates
               Actions = actions @ createResourceActions cfg
               RootPath = path
               OutputRoot = outputRoot }: Context)

    let load (path: string) (outputRoot: string) (args: Map<string, string>) =
        File.ReadAllText
        <| Path.Combine(path, "fxd.json")
        |> JsonSerializer.Deserialize<Configuration>
        |> Internal.createContext path outputRoot args

    let run (ctx: Context) =
        ctx.Actions
        |> List.iter
            (fun a ->
                match a with
                | GeneratePage gpa ->
                    Internal.generatePage ctx.Templates ctx.RootPath (Path.Combine(ctx.OutputRoot, gpa.Output)) gpa
                    |> fun r -> printfn $"*** {r}"
                | GenerateCodeDocument gcda ->
                    // Extract the source data.
                    SourceExtractor.extract gcda.Source
                    |> List.map
                        (fun m ->
                            match m with
                            | Member.Class cd ->
                                match ctx.Templates.Values.TryFind gcda.ClassTemplate with
                                | Some t ->
                                    let metadata =
                                       gcda.Metadata
                                       |> Map.toList
                                       |> List.map (fun (k, v) -> k, Mustache.Value.Scalar v)
                                    
                                    Templating.createClassValues metadata cd
                                    |> fun v -> ({ Values = v; Partials = Map.empty }: Mustache.Data)
                                    |> fun d -> Mustache.replace d true t
                                    |> fun r ->
                                        File.WriteAllText(Path.Combine(ctx.OutputRoot, gcda.DirectoryName, $"{cd.Id}.html"), r)
                                | None -> ()
                                |> ignore
                            | Member.Function fd -> ()
                            | Member.Module md -> ()
                            | Member.Namespace ns -> ()
                            | Member.Record rd -> ()
                            | Member.Union ud -> ())
                    |> ignore
                | CreateDirectory cda ->
                    printfn $"Creating directory `{cda.Path}`."
                    
                    Directory.CreateDirectory(Path.Combine(ctx.OutputRoot, cda.Path))
                    |> ignore
                | CopyFile cfa ->
                    printfn $"Copying file `{cfa.From}` to `{cfa.To}`."
                    File.Copy(Path.Combine(ctx.RootPath, cfa.From), (Path.Combine(ctx.OutputRoot, cfa.To))))