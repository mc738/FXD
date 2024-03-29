﻿namespace FXD.CodeDocuments.FSharp

module Documentation =

    open System.Text.RegularExpressions
    open FSharp.Compiler.Symbols
    open FXD
    open XmlDocExtractor

    type FunctionDocument =
        { Id: string
          DisplayName: string
          Namespace: string
          Signature: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Path: string
          Parameters: FunctionParameter list }

        static member Create(entity: FSharpMemberOrFunctionOrValue, path: string, ns: string) =
            let doc =
                extractXmlDoc entity.DisplayName entity.XmlDoc

            { Id = slugifyName entity.FullName
              DisplayName = entity.DisplayName
              Namespace = ns
              Signature = entity.FullType.Format displayContext
              XDocSignature = entity.XmlDocSig
              XmlDocument = doc
              Path = path
              Parameters =
                entity.CurriedParameterGroups
                |> listMap (fun pl ->
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
            let name =
                entity.Name |> Option.defaultValue ""

            { Id = slugifyName entity.FullName
              Name = name
              Type = entity.Type.Format displayContext
              Document =
                xmlDocMember
                |> Option.bind (fun d ->
                    d.Parameters
                    |> List.tryFind (fun p -> p.Name = name)
                    |> Option.bind (fun p -> Some p.Description)) }

    and UnionDocument =
        { Id: string
          FullName: string
          DisplayName: string
          Namespace: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Path: string
          Members: UnionMember list
          Properties: PropertyDocument list
          Methods: MethodDocument list }

        static member Create(entity: FSharpEntity, path: string, properties: PropertyDocument list, methods: MethodDocument list) =
            { Id = slugifyName entity.FullName
              FullName = entity.FullName
              DisplayName = entity.DisplayName
              Namespace = entity.Namespace |> Option.defaultValue ""
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
              Path = path
              Members =
                entity.UnionCases
                |> listMap (fun e -> UnionMember.Create(e))
              Properties = properties
              Methods = methods }
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
          Namespace: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Path: string
          Fields: RecordField list
          Properties: PropertyDocument list
          Methods: MethodDocument list }

        static member Create(entity: FSharpEntity, path: string, properties: PropertyDocument list, methods: MethodDocument list) =
            let dm =
                extractXmlDoc entity.DisplayName entity.XmlDoc

            { Id = slugifyName entity.FullName
              FullName = entity.FullName
              DisplayName = entity.DisplayName
              Namespace = entity.Namespace |> Option.defaultValue ""
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
              Path = path
              Fields =
                entity.FSharpFields
                |> List.ofSeq
                |> List.map (fun f -> RecordField.Create(f))
              Properties = properties
              Methods = methods }
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
          Namespace: string
          XmlDocument: XmlDocumentMember option
          Path: string
          Properties: PropertyDocument list
          Methods: MethodDocument list }

        static member Create(entity: FSharpEntity, path: string, properties: PropertyDocument list, methods: MethodDocument list) =
            { Id = slugifyName entity.FullName
              FullName = entity.FullName
              DisplayName = entity.DisplayName
              Namespace = entity.Namespace |> Option.defaultValue ""
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
              Path = path
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
          Namespace: string
          DisplayName: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Path: string
          Members: Member list }

        static member Create(entity: FSharpEntity, path: string, members: Member list) =
            { Id = slugifyName entity.FullName
              FullName = entity.FullName
              Namespace = entity.Namespace |> Option.defaultValue ""
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
              Path = path
              Members = members }
            |> Member.Module

        static member CreateTopLevel(id, fullName, ns, displayName, path, members) =
            { Id = id
              FullName = fullName
              Namespace = ns
              DisplayName = displayName
              XDocSignature = ""
              XmlDocument = None
              Path = path
              Members = members }

        member m.GetClasses() =
            m.Members
            |> List.fold
                (fun acc m ->
                    match m with
                    | Member.Class c -> acc @ [ c ]
                    | _ -> acc)
                []

        member m.GetRecords() =
            m.Members
            |> List.fold
                (fun acc m ->
                    match m with
                    | Member.Record r -> acc @ [ r ]
                    | _ -> acc)
                []

        member m.GetUnions() =
            m.Members
            |> List.fold
                (fun acc m ->
                    match m with
                    | Member.Union u -> acc @ [ u ]
                    | _ -> acc)
                []

        member m.GetFunctions() =
            m.Members
            |> List.fold
                (fun acc m ->
                    match m with
                    | Member.Function f -> acc @ [ f ]
                    | _ -> acc)
                []

        member m.GetModules() =
            m.Members
            |> List.fold
                (fun acc m ->
                    match m with
                    | Member.Module m -> acc @ [ m ]
                    | _ -> acc)
                []

    and NamespaceDocument = { Members: Member list }

    and AbbreviatedDocument =
        { Id: string
          FullName: string
          DisplayName: string
          Type: string
          Namespace: string }

    and [<RequireQualifiedAccess>] Member =
        | Function of FunctionDocument
        | Union of UnionDocument
        | Record of RecordDocument
        | Class of ClassDocument
        | Module of ModuleDocument
        | Namespace of NamespaceDocument
        | Abbreviation of AbbreviatedDocument

        member m.GetNamespace() =
            match m with
            | Function f -> f.Namespace
            | Union u -> u.Namespace
            | Record r -> r.Namespace
            | Class c -> c.Namespace
            | Module m -> m.Namespace
            | Namespace n -> failwith "TODO: implement."
            | Abbreviation a -> a.Namespace

        member m.MatchName(regex: string) =
            match m with
            | Function f -> f.DisplayName
            | Union u -> u.FullName
            | Record r -> r.FullName
            | Class c -> c.FullName
            | Module m -> m.FullName
            | Namespace n -> failwith "TODO: implement."
            | Abbreviation a -> ""
            |> fun v -> Regex.IsMatch(v, regex)

        member m.GetId() =
            match m with
            | Function f -> f.Id
            | Union u -> u.Id
            | Record r -> r.Id
            | Class c -> c.Id
            | Module m -> m.Id
            | Namespace ns -> failwith "TODO: implement."
            | Abbreviation a -> a.Id

        member m.GetDisplayName() =
            match m with
            | Function f -> f.DisplayName
            | Union u -> u.DisplayName
            | Record r -> r.DisplayName
            | Class c -> c.DisplayName
            | Module m -> m.DisplayName
            | Namespace ns -> failwith "TODO: implement."
            | Abbreviation a -> a.DisplayName

        member m.GetPath() =
            match m with
            | Function f -> f.Path
            | Union u -> u.Path
            | Record r -> r.Path
            | Class c -> c.Path
            | Module m -> m.Path
            | Namespace ns -> failwith "TODO: implement."
            | Abbreviation a -> failwith "TODO: implement."
            