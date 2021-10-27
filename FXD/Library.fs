namespace FXD

open System
open System.IO
open System.Reflection.Metadata
open System.Xml.Linq
open System.Xml.Linq
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Xml

[<AutoOpen>]
module Common =
    let listMap<'In, 'Out> (fn: 'In -> 'Out) (s: seq<'In>) = s |> List.ofSeq |> List.map fn

module Documentation =

    type XmlParameter = { Name: string; Description: string }

    type XmlDocumentMember =
        { Name: string
          Summary: string option
          Parameters: XmlParameter list
          Returns: string option
          Examples: string list }

    type FunctionDocument =
        { DisplayName: string
          Signature: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Parameters: FunctionParameter list }

        static member Create(entity: FSharpMemberOrFunctionOrValue, docMembers: Map<string, XmlDocumentMember>) =
            let doc = docMembers.TryFind entity.XmlDocSig

            { DisplayName = entity.DisplayName
              Signature = entity.FullType.ToString()
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
        { Name: string
          Type: string
          Document: string option }

        static member Create(entity: FSharpParameter, xmlDocMember: XmlDocumentMember option) =
            let name = entity.Name |> Option.defaultValue ""

            { Name = name
              Type = entity.Type.ToString()
              Document =
                  xmlDocMember
                  |> Option.bind
                      (fun d ->
                          d.Parameters
                          |> List.tryFind (fun p -> p.Name = name)
                          |> Option.bind (fun p -> Some p.Description)) }

    and UnionDocument =
        { FullName: string
          DisplayName: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Members: UnionMember list }

        static member Create(entity: FSharpEntity, docMembers: Map<string, XmlDocumentMember>) =
            let doc = docMembers.TryFind entity.XmlDocSig

            { FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              XmlDocument = doc
              Members =
                  entity.UnionCases
                  |> listMap (fun e -> UnionMember.Create(e, docMembers)) }
            |> Member.Union

    and UnionMember =
        { Name: string
          Type: string
          XmlDocument: XmlDocumentMember option }

        static member Create(unionCase: FSharpUnionCase, docMembers: Map<string, XmlDocumentMember>) =
            { Name = unionCase.Name
              Type = unionCase.ReturnType.ToString()
              XmlDocument = docMembers.TryFind unionCase.XmlDocSig }

    and RecordDocument =
        { FullName: string
          DisplayName: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Fields: RecordField list }

        static member Create(entity: FSharpEntity, docMembers: Map<string, XmlDocumentMember>) =
            { FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              XmlDocument = docMembers.TryFind entity.XmlDocSig
              Fields =
                  entity.FSharpFields
                  |> List.ofSeq
                  |> List.map (fun f -> RecordField.Create(f, docMembers)) }
            |> Member.Record

    and RecordField =
        { Name: string
          Type: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option }

        static member Create(field: FSharpField, docMembers: Map<string, XmlDocumentMember>) =
            { Name = field.Name
              Type = field.FieldType.ToString()
              XDocSignature = field.XmlDocSig
              XmlDocument = docMembers.TryFind field.XmlDocSig }

    and ClassDocument =
        { FullName: string
          DisplayName: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Members: Member list }

        static member Create(entity: FSharpEntity, children: Member list, docMembers: Map<string, XmlDocumentMember>) =
            { FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              XmlDocument = docMembers.TryFind entity.XmlDocSig
              Members = children }
            |> Member.Class

    and ModuleDocument =
        { FullName: string
          DisplayName: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Members: Member list }

        static member Create(entity: FSharpEntity, members: Member list, docMembers: Map<string, XmlDocumentMember>) =
            { FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              XmlDocument = docMembers.TryFind entity.XmlDocSig
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

module XmlDocExtractor =

    open Documentation

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

    let extract path =
        let root = load path

        match root |> getElement "members" with
        | Some el ->
            el.Elements(xName "member")
            |> listMap
                (fun mel ->
                    { Name =
                          mel
                          |> getAttribute "name"
                          |> Option.map (fun at -> at.Value)
                          |> Option.defaultValue ""
                      Summary =
                          mel
                          |> getElement "summary"
                          |> Option.map (fun el -> el.Value)
                      Parameters =
                          mel.Elements(xName "param")
                          |> listMap
                              (fun pel ->
                                  { Name =
                                        pel
                                        |> getAttribute "name"
                                        |> Option.map (fun at -> at.Value)
                                        |> Option.defaultValue ""
                                    Description = pel.Value })
                      Returns =
                          mel
                          |> getElement "returns"
                          |> Option.map (fun el -> el.Value)
                      Examples =
                          mel.Elements(xName "example")
                          |> listMap (fun eel -> eel.Value) })
            |> List.map (fun m -> m.Name, m)
            |> Map.ofList
        | None -> failwith "Error"

/// Extract information from source code.
module SourceExtractor =

    open Documentation

    let load (path: string) = File.ReadAllText(path)

    let parseAndCheckScript path =
        let checker =
            FSharpChecker.Create(keepAssemblyContents = true)

        let text = load path

        let projOptions, errors =
            checker.GetProjectOptionsFromScript(path, SourceText.ofString text)
            |> Async.RunSynchronously

        printfn $"Errors: {errors.Length}"

        checker.ParseAndCheckProject(projOptions)
        |> Async.RunSynchronously

    let createFunctions (docMembers: Map<string, XmlDocumentMember>) (entity: FSharpEntity) =
        entity.MembersFunctionsAndValues
        |> listMap (fun m -> FunctionDocument.Create(m, docMembers))

    let rec create (docMembers: Map<string, XmlDocumentMember>) (entity: FSharpEntity) =
        match entity with
        | _ when entity.IsFSharpModule ->
            ModuleDocument.Create(
                entity,
                [ entity.NestedEntities
                  |> listMap (create docMembers)
                  entity.MembersFunctionsAndValues
                  |> listMap (fun m -> FunctionDocument.Create(m, docMembers)) ]
                |> List.concat,
                docMembers
            )
        | _ when entity.IsFSharpRecord -> RecordDocument.Create(entity, docMembers)
        | _ when entity.IsFSharpUnion -> UnionDocument.Create(entity, docMembers)
        | _ when entity.IsClass ->
            ClassDocument.Create(
                entity,
                entity.NestedEntities
                |> listMap (create docMembers),
                docMembers
            )
        | _ when entity.IsArrayType -> failwith "todo"
        | _ when entity.IsNamespace -> failwith "todo"
        //| _ when
        | _ -> failwith "???"

    let extract (path: string) (docMembers: Map<string, XmlDocumentMember>) =
        let s = parseAndCheckScript path
        let assembly = s.AssemblySignature

        assembly.Entities
        |> List.ofSeq
        |> List.map (create docMembers)

[<RequireQualifiedAccess>]
module Linter =

    open Documentation

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

                let r2 =
                    cd.Members |> List.map handler |> List.concat

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