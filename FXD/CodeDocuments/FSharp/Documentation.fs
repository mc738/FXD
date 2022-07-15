namespace FXD.CodeDocuments.FSharp

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
          Parameters: FunctionParameter list }

        static member Create(entity: FSharpMemberOrFunctionOrValue, ns: string) =
            let doc =
                extractXmlDoc entity.DisplayName entity.XmlDoc

            { Id = slugifyName entity.FullName
              DisplayName = entity.DisplayName
              Namespace = ns
              Signature = entity.FullType.Format displayContext
              XDocSignature = entity.XmlDocSig
              XmlDocument = doc
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
          Members: UnionMember list
          Properties: PropertyDocument list
          Methods: MethodDocument list }

        static member Create(entity: FSharpEntity, properties: PropertyDocument list, methods: MethodDocument list) =
            { Id = slugifyName entity.FullName
              FullName = entity.FullName
              DisplayName = entity.DisplayName
              Namespace = entity.Namespace |> Option.defaultValue ""
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
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
          Fields: RecordField list
          Properties: PropertyDocument list
          Methods: MethodDocument list }

        static member Create(entity: FSharpEntity, properties: PropertyDocument list, methods: MethodDocument list) =
            let dm =
                extractXmlDoc entity.DisplayName entity.XmlDoc

            { Id = slugifyName entity.FullName
              FullName = entity.FullName
              DisplayName = entity.DisplayName
              Namespace = entity.Namespace |> Option.defaultValue ""
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
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
          Properties: PropertyDocument list
          Methods: MethodDocument list }

        static member Create(entity: FSharpEntity, properties: PropertyDocument list, methods: MethodDocument list) =
            { Id = slugifyName entity.FullName
              FullName = entity.FullName
              DisplayName = entity.DisplayName
              Namespace = entity.Namespace |> Option.defaultValue ""
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
          Namespace: string
          DisplayName: string
          XDocSignature: string
          XmlDocument: XmlDocumentMember option
          Members: Member list }

        static member Create(entity: FSharpEntity, members: Member list) =
            { Id = slugifyName entity.FullName
              FullName = entity.FullName
              Namespace = entity.Namespace |> Option.defaultValue ""
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              XmlDocument = extractXmlDoc entity.DisplayName entity.XmlDoc
              Members = members }
            |> Member.Module

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
