namespace FXD

open System
open System.IO
open System.Reflection.Metadata
open System.Xml
open System.Xml.Linq
open System.Xml.Linq
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Xml

[<AutoOpen>]
module Common =
    let listMap<'In, 'Out> (fn: 'In -> 'Out) (s: seq<'In>) = s |> List.ofSeq |> List.map fn
       
    let displayContext =
        FSharpDisplayContext.Empty
        |> fun ctx -> ctx.WithSuffixGenericParameters()
        |> fun ctx -> ctx.WithShortTypeNames true
    
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



module XmlDocExtractor =


    type XmlParameter = { Name: string; Description: string }

    type XmlDocumentMember =
        { Name: string
          Summary: string option
          Parameters: XmlParameter list
          Returns: string option
          Examples: string list }

    [<AutoOpen>]
    module Helpers =

        let xName (name: string) = XName.op_Implicit name

        let getElement (name: string) (el: XElement) =
            let found = el.Element(xName name)

            match found <> null with
            | true -> Some found
            | false -> None

        let getAttribute (name: string) (el: XElement) =
            let found = el.Attribute(xName name)

            match found <> null with
            | true -> Some found
            | false -> None

    let load (path: string) =
        File.ReadAllText path
        |> XDocument.Parse
        |> fun xdoc -> xdoc.Root

    let extractMember (el: XElement) =
        { Name =
              el
              |> getAttribute "name"
              |> Option.map (fun at -> at.Value)
              |> Option.defaultValue ""
          Summary =
              el
              |> getElement "summary"
              |> Option.map (fun el -> el.Value)
          Parameters =
              el.Elements(xName "param")
              |> listMap
                  (fun pel ->
                      { Name =
                            pel
                            |> getAttribute "name"
                            |> Option.map (fun at -> at.Value)
                            |> Option.defaultValue ""
                        Description = pel.Value })
          Returns =
              el
              |> getElement "returns"
              |> Option.map (fun el -> el.Value)
          Examples =
              el.Elements(xName "example")
              |> listMap (fun eel -> eel.Value) }

    let extract path =
        let root = load path

        match root |> getElement "members" with
        | Some el ->
            el.Elements(xName "member")
            |> listMap (fun mel -> extractMember mel)
            |> List.map (fun m -> m.Name, m)
            |> Map.ofList
        | None -> failwith "Error"

    let extractXmlDoc (name: string) (doc: FSharpXmlDoc) =
        match doc with
        | FSharpXmlDoc.FromXmlText txt ->
            match txt.UnprocessedLines.Length > 0 with
            | true ->
                try
                    let xml =
                        txt.UnprocessedLines
                        |> String.concat ""
                        |> fun b -> $"<member name=\"{name}\">{b}</member>"

                    extractMember (XElement.Parse(xml)) |> Some
                with
                | _ -> None
            | false -> None
        | FSharpXmlDoc.FromXmlFile _ -> None
        | FSharpXmlDoc.None _ -> None

module Documentation =

    open XmlDocExtractor

    type FunctionDocument =
        { Id: string
          DisplayName: string
          Signature: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Parameters: FunctionParameter list }

        static member Create(entity: FSharpMemberOrFunctionOrValue) =
            let doc =
                extractXmlDoc entity.DisplayName entity.XmlDoc

            { Id = slugifyName entity.FullName 
              DisplayName = entity.DisplayName
              Signature = entity.FullType.Format displayContext
              XDocSignature = entity.XmlDocSig
              XmlDocument = doc
              Parameters =
                  entity.CurriedParameterGroups
                  |> listMap
                      (fun pl ->
                          pl
                          |> listMap (fun p -> FunctionParameter.Create(p, doc)))
                  |> List.concat }
            |> Member.Function

    and FunctionParameter =
        { Id: string
          Name: string
          Type: string
          Document: string option }

        static member Create(entity: FSharpParameter, xmlDocMember: XmlDocumentMember option) =
            let name = entity.Name |> Option.defaultValue ""

            { Id = slugifyName entity.FullName
              Name = name
              Type = entity.Type.Format displayContext
              Document =
                  xmlDocMember
                  |> Option.bind
                      (fun d ->
                          d.Parameters
                          |> List.tryFind (fun p -> p.Name = name)
                          |> Option.bind (fun p -> Some p.Description)) }

    and UnionDocument =
        { Id: string
          FullName: string
          DisplayName: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Members: UnionMember list }

        static member Create(entity: FSharpEntity) =
            { Id = slugifyName entity.FullName
              FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
              Members =
                  entity.UnionCases
                  |> listMap (fun e -> UnionMember.Create(e)) }
            |> Member.Union

    and UnionMember =
        { Id: string
          Name: string
          Type: string
          XmlDocument: XmlDocumentMember option }

        static member Create(unionCase: FSharpUnionCase) =
            { Id = slugifyName unionCase.FullName 
              Name = unionCase.Name
              Type = unionCase.ReturnType.Format displayContext
              XmlDocument = extractXmlDoc unionCase.Name unionCase.XmlDoc }

    and RecordDocument =
        { Id: string
          FullName: string
          DisplayName: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Fields: RecordField list }

        static member Create(entity: FSharpEntity) =
            let dm =
                extractXmlDoc entity.DisplayName entity.XmlDoc

            { Id = slugifyName entity.FullName 
              FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
              Fields =
                  entity.FSharpFields
                  |> List.ofSeq
                  |> List.map (fun f -> RecordField.Create(f)) }
            |> Member.Record

    and RecordField =
        { Id: string
          Name: string
          Type: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option }

        static member Create(field: FSharpField) =
            { Id = slugifyName field.FullName
              Name = field.Name
              Type = field.FieldType.Format displayContext
              XDocSignature = field.XmlDocSig
              XmlDocument = extractXmlDoc field.DisplayName field.XmlDoc }

    and ClassDocument =
        { Id: string
          FullName: string
          DisplayName: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Properties: PropertyDocument list
          Methods: MethodDocument list }

        static member Create
            (
                entity: FSharpEntity,
                properties: PropertyDocument list,
                methods: MethodDocument list
            ) =
            { Id = slugifyName entity.FullName 
              FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
              Properties = properties
              Methods = methods }
            |> Member.Class

    and PropertyDocument =
        { Id: string
          Name: string
          Type: string
          XmlDocument: XmlDocumentMember option }

    and MethodDocument =
        { Id: string 
          Name: string
          Signature: string
          Parameters: FunctionParameter list
          XmlDocument: XmlDocumentMember option }

    and ModuleDocument =
        { Id: string
          FullName: string
          DisplayName: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Members: Member list }

        static member Create(entity: FSharpEntity, members: Member list) =
            { Id = slugifyName entity.FullName
              FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
              Members = members }
            |> Member.Module

    and NamespaceDocument = { Members: Member list }

    and [<RequireQualifiedAccess>] Member =
        | Function of FunctionDocument
        | Union of UnionDocument
        | Record of RecordDocument
        | Class of ClassDocument
        | Module of ModuleDocument
        | Namespace of NamespaceDocument

open Documentation

/// Extract information from source code.
module SourceExtractor =

    open Documentation
    open XmlDocExtractor

    let load (path: string) = File.ReadAllText(path)

    let parseAndCheckScript path =
        let checker =
            FSharpChecker.Create(keepAssemblyContents = true)

        let options =
            { FSharpParsingOptions.Default with
                  SourceFiles = [| path |] }

        let text = load path

        //checker.CompileToDynamicAssembly()
        //let parseFile = checker.ParseFile(path, SourceText.ofString text, options) |> Async.RunSynchronously

        let projOptions, errors =
            checker.GetProjectOptionsFromScript(path, SourceText.ofString text, assumeDotNetFramework = false)
            |> Async.RunSynchronously

        printfn $"Errors: {errors.Length}"

        checker.ParseAndCheckProject(projOptions)
        |> Async.RunSynchronously

    let createFunctions (docMembers: Map<string, XmlDocumentMember>) (entity: FSharpEntity) =
        entity.MembersFunctionsAndValues
        |> listMap (fun m -> FunctionDocument.Create(m))

    let rec create (entity: FSharpEntity) =
        match entity with
        | _ when entity.IsFSharpModule ->
            ModuleDocument.Create(
                entity,
                [ entity.NestedEntities
                  |> listMap create
                  entity.MembersFunctionsAndValues
                  |> listMap (fun m -> FunctionDocument.Create(m)) ]
                |> List.concat
            )
        | _ when entity.IsFSharpRecord -> RecordDocument.Create(entity)
        | _ when entity.IsFSharpUnion -> UnionDocument.Create(entity)
        | _ when entity.IsClass ->
            let createMethod (v: FSharpMemberOrFunctionOrValue) =
                let dm = extractXmlDoc v.DisplayName v.XmlDoc

                ({ Id = slugifyName v.FullName
                   Name = v.DisplayName
                   Signature = v.FullType.Format displayContext
                   Parameters =
                       v.CurriedParameterGroups
                       |> listMap
                           (fun pl ->
                               pl
                               |> listMap (fun p -> FunctionParameter.Create(p, dm)))
                       |> List.concat
                   XmlDocument = extractXmlDoc v.DisplayName v.XmlDoc }: MethodDocument)

            let createProperty (v: FSharpMemberOrFunctionOrValue) =
                ({ Id = slugifyName v.FullName
                   Name = v.DisplayName
                   Type = v.FullType.Format displayContext
                   XmlDocument = extractXmlDoc v.DisplayName v.XmlDoc }: PropertyDocument)

            let (methods, properties) =
                entity.MembersFunctionsAndValues
                |> List.ofSeq
                |> List.fold
                    (fun (m, p) e ->
                        match e.IsProperty with
                        | true -> (m, p @ [ createProperty e ])
                        | false -> (m @ [ createMethod e ], p))
                    ([], [])
            ClassDocument.Create(entity, properties, methods)
        | _ when entity.IsArrayType -> failwith "todo"
        | _ when entity.IsNamespace -> failwith "todo"
        //| _ when
        | _ -> failwith "???"

    let extract (path: string) =
        let s = parseAndCheckScript path
        let assembly = s.AssemblySignature

        assembly.Entities
        |> List.ofSeq
        |> List.map (create)

[<RequireQualifiedAccess>]
module Linter =

    open Documentation
    open XmlDocExtractor

    type LintingError =
        | MissingXmlDocMember of string
        | MissingXmlDocSummary of string
        | MissingXmlDocParameter of string
        | MissingXmlDocReturn of string
        | MissingXmlDocExample of string
        | UnknownType of string

    type LintSettings =
        { SummaryRequired: bool
          ParametersRequired: bool
          ReturnsRequired: bool
          ExamplesRequired: bool }

        static member Strict =
            { SummaryRequired = true
              ParametersRequired = true
              ReturnsRequired = true
              ExamplesRequired = true }

    type LintingResult =
        { Members: Member list
          LintErrors: LintingError list }

    let lintXmlDocument (settings: LintSettings) (doc: XmlDocumentMember) =
        Some [ match doc.Summary with
               | Some _ -> ()
               | None ->
                   if settings.SummaryRequired then
                       yield LintingError.MissingXmlDocSummary doc.Name

               match doc.Parameters.Length > 0 with
               | true -> ()
               | false ->
                   if settings.ParametersRequired then
                       yield LintingError.MissingXmlDocParameter doc.Name

               match doc.Returns with
               | Some _ -> ()
               | None ->
                   if settings.ReturnsRequired then
                       yield LintingError.MissingXmlDocReturn doc.Name

               match doc.Examples.Length > 0 with
               | true -> ()
               | false ->
                   if settings.ExamplesRequired then
                       yield LintingError.MissingXmlDocExample doc.Name ]

    let run (errorsHandler: Member list -> LintingError list -> Member list) (members: Member list) =

        let rec handler (m: Member) =
            match m with
            | Member.Function fd ->
                fd.XmlDocument
                |> Option.bind (lintXmlDocument LintSettings.Strict)
                |> Option.defaultValue [ MissingXmlDocMember fd.DisplayName ]

            | Member.Union ud ->
                let r1 =
                    ud.XmlDocument
                    |> Option.bind (lintXmlDocument LintSettings.Strict)
                    |> Option.defaultValue [ MissingXmlDocMember ud.DisplayName ]

                let r2 =
                    ud.Members
                    |> List.map
                        (fun um ->
                            um.XmlDocument
                            |> Option.bind (lintXmlDocument LintSettings.Strict)
                            |> Option.defaultValue [ MissingXmlDocMember um.Name ])
                    |> List.concat

                r1 @ r2
            | Member.Record rd ->
                let r1 =
                    rd.XmlDocument
                    |> Option.bind (lintXmlDocument LintSettings.Strict)
                    |> Option.defaultValue [ MissingXmlDocMember rd.DisplayName ]

                let r2 =
                    rd.Fields
                    |> List.map
                        (fun rf ->
                            rf.XmlDocument
                            |> Option.bind (lintXmlDocument LintSettings.Strict)
                            |> Option.defaultValue [ MissingXmlDocMember rf.Name ])
                    |> List.concat

                r1 @ r2
            | Member.Class cd ->
                let r1 =
                    cd.XmlDocument
                    |> Option.bind (lintXmlDocument LintSettings.Strict)
                    |> Option.defaultValue [ MissingXmlDocMember cd.DisplayName ]

                let r2 = []

                r1 @ r2
            | Member.Module md ->
                let r1 =
                    md.XmlDocument
                    |> Option.bind (lintXmlDocument LintSettings.Strict)
                    |> Option.defaultValue [ MissingXmlDocMember md.DisplayName ]

                let r2 =
                    md.Members |> List.map handler |> List.concat

                r1 @ r2
            | Member.Namespace nd -> nd.Members |> List.map handler |> List.concat

        errorsHandler members (members |> List.map handler |> List.concat)