namespace FXD.CodeDocuments.FSharp

open FXD.CodeDocuments.FSharp.Documentation

module SourceExtractor =

    open System.IO
    open FSharp.Compiler.CodeAnalysis
    open FSharp.Compiler.Symbols
    open FSharp.Compiler.Text
    open FXD
    open Documentation
    open XmlDocExtractor

    let load (path: string) = File.ReadAllText(path)

    let parseAndCheckScript path =
        let checker =
            FSharpChecker.Create(keepAssemblyContents = true)

        let text = load path

        let projOptions, errors =
            checker.GetProjectOptionsFromScript(path, SourceText.ofString text, assumeDotNetFramework = false)
            |> Async.RunSynchronously

        printfn $"Errors: {errors.Length}"

        checker.ParseAndCheckProject(projOptions)
        |> Async.RunSynchronously

    let createFunctions (docMembers: Map<string, XmlDocumentMember>) (entity: FSharpEntity) =
        entity.MembersFunctionsAndValues
        |> listMap (fun m -> FunctionDocument.Create(m, entity.Namespace |> Option.defaultValue ""))

    let createMethod (v: FSharpMemberOrFunctionOrValue) =
        let dm =
            extractXmlDoc v.DisplayName v.XmlDoc

        ({ Id = slugifyName v.FullName
           Name = v.DisplayName
           Signature = v.FullType.Format displayContext
           Parameters =
             v.CurriedParameterGroups
             |> listMap (fun pl ->
                 pl
                 |> listMap (fun p -> FunctionParameter.Create(p, dm)))
             |> List.concat
           XmlDocument = extractXmlDoc v.DisplayName v.XmlDoc }: MethodDocument)

    let createProperty (v: FSharpMemberOrFunctionOrValue) =
        ({ Id = slugifyName v.FullName
           Name = v.DisplayName
           Type = v.FullType.Format displayContext
           XmlDocument = extractXmlDoc v.DisplayName v.XmlDoc }: PropertyDocument)

    let createPropertiesAndMethods (entity: FSharpEntity) =
        entity.MembersFunctionsAndValues
        |> List.ofSeq
        |> List.fold
            (fun (p, m) e ->
                match e.IsProperty with
                | true -> (p @ [ createProperty e ], m)
                | false -> (p, m @ [ createMethod e ]))
            ([], [])

    let rec create (path: string) (entity: FSharpEntity) =
        match entity with
        | _ when entity.IsFSharpModule ->
            ModuleDocument.Create(
                entity,
                path,
                [ entity.NestedEntities |> listMap (create path)
                  entity.MembersFunctionsAndValues
                  |> listMap (fun m -> FunctionDocument.Create(m, entity.Namespace |> Option.defaultValue "")) ]
                |> List.concat
            )
        | _ when entity.IsFSharpRecord ->
            let (properties, methods) =
                createPropertiesAndMethods entity

            RecordDocument.Create(entity, properties, methods)
        | _ when entity.IsFSharpUnion ->
            let (properties, methods) =
                createPropertiesAndMethods entity

            UnionDocument.Create(entity, properties, methods)
        | _ when entity.IsClass ->
            let createMethod (v: FSharpMemberOrFunctionOrValue) =
                let dm =
                    extractXmlDoc v.DisplayName v.XmlDoc

                ({ Id = slugifyName v.FullName
                   Name = v.DisplayName
                   Signature = v.FullType.Format displayContext
                   Parameters =
                     v.CurriedParameterGroups
                     |> listMap (fun pl ->
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
        | _ when entity.IsFSharpAbbreviation ->
            // TODO handle better.
            let fullName =
                $"{entity.Namespace}.{entity.CompiledName}"

            ({ Id = slugifyName fullName
               FullName = fullName
               DisplayName = entity.DisplayName
               Type = entity.AbbreviatedType.ToString()
               Namespace = entity.Namespace |> Option.defaultValue "" }: AbbreviatedDocument)
            |> Member.Abbreviation
        //| _ when
        | _ -> failwith "???"

    let extract (rootPath: string) (path: string) =
        let s = parseAndCheckScript path
        let assembly = s.AssemblySignature

        assembly.Entities
        |> List.ofSeq
        |> List.map (create <| Path.GetRelativePath(rootPath, path))

    let extractMultiple (rootPath: string) (paths: string list) (filterRegex: string) =
        
        paths
        |> List.map (fun p -> extract rootPath p)
        |> List.concat
        |> List.choose (fun m ->
            match m.MatchName filterRegex |> not with
            | true -> Some (m)
            | false -> None)
        |> List.groupBy (fun r -> r.GetNamespace())
        |> Map.ofList
        
        
    let group (members: Member list) =
        // TODO Handle top level non module.
        let (modules, topLevel) =
            members
            |> List.fold
                (fun (acc, topLevel) m ->
                    match m with
                    | Member.Module mm -> acc @ [ mm ], topLevel
                    | _ -> acc, topLevel @ [])
                ([], [])
                
        modules
