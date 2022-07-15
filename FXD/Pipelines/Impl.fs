namespace FXD.Pipelines

open System.IO
open FDOM.Core.Parsing
open FDOM.Rendering
open FXD
open FXD.Pipelines.Configuration
open Fluff.Core

module Impl =

    open FXD
    (*
    module Internal =

        open FDOM

        let generatePage
            (template: Mustache.Token list)
            (rootPath: string)
            (outputPath: string)
            (source: string)
            (title: string)
            //(action: GeneratePageAction)
            (indexes: IndexSection list)
            =
                let blocks =
                    Parser
                        .ParseLines(
                            File.ReadAllLines(Path.Combine(rootPath, source))
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


                (*
                // Create the indexes
                let pageIndexes =
                    Html.getIndexes doc
                    |> List.map
                        (fun i ->
                            [ "index_slug", i |> slugifyName |> Mustache.Value.Scalar
                              "index_title", i |> Mustache.Value.Scalar ]
                            |> Map.ofList
                            |> Mustache.Value.Object)
                    |> Mustache.Value.Array
                *)

                // Merge page indexes with generate indexes.
                let indexesHtml =
                    indexes
                    |> List.map (fun i ->
                        let pagesIndexHtml =
                            i.Pages
                            |> List.map (fun pi ->
                                match pi.Name with
                                | _ when String.Equals(pi.Name, action.TitleSlug) ->
                                    // Generate the open index.
                                    let content =
                                        Html.getIndexes doc
                                        |> List.map (fun i -> $"""<a href="#{i |> slugifyName}">{i}</a>""")
                                        |> String.concat ""

                                    $"""<li class="open"><h3>{pi.Title}</h3>{content}</li>"""
                                | _ -> $"""<li><a href="./{pi.Name}.html">{pi.Title}</a></li>""")
                            |> String.concat ""

                        $"""<div class="index-section"><h2>{i.SectionName}</h2><ul>{pagesIndexHtml}</ul></div>""")
                    |> String.concat ""

                let values =
                    ({ Values =
                        [ "title", Mustache.Value.Scalar action.Title
                          "titleSlug", Mustache.Value.Scalar action.TitleSlug
                          "now", Mustache.Value.Scalar(DateTime.Now.ToString("dd MMMM yyyy HH:mm:ss"))
                          "index", Mustache.Value.Scalar indexesHtml ]
                        @ (action.Metadata
                           |> Map.toList
                           |> List.map (fun (k, v) -> k, Mustache.Value.Scalar v))
                        |> Map.ofList
                       Partials = Map.empty }: Mustache.Data)

                Html.renderFromParsedTemplate template values [] [] doc
                |> fun r -> File.WriteAllText(outputPath, r)
                |> Ok

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

        let createPageActions
            (args: Map<string, string>)
            (globalMetadata: Map<string, string>)
            (rootPath: string)
            (cfg: Configuration)
            (acc: Action list)
            =
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
                              TitleSlug = substituteValue args p.TitleSlug
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
                |> List.map (fun cs ->
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

            let globalMetadata =
                cfg.Metadata |> createMetadata argsV Map.empty

            let (actions, templates) =
                cfg.Directories
                |> List.ofSeq
                |> createDirectoryActions
                |> createPageActions argsV globalMetadata path cfg
                |> createCodeDocumentActions globalMetadata argsV path cfg

            let indexes =
                cfg.Indexes
                |> List.ofSeq
                |> List.map (fun i ->
                    let t =
                        match i.Type.ToLower() with
                        | "pages" -> IndexSectionType.Pages
                        | "codedocuments" -> IndexSectionType.CodeDocuments
                        | _ -> failwith $"Unknown index section type: `{i.SectionName}`"

                    ({ SectionName = i.SectionName
                       Type = t
                       Pages =
                         i.Pages
                         |> List.ofSeq
                         |> List.map (fun pi ->
                             ({ Name = pi
                                Title =
                                  match t with
                                  | IndexSectionType.Pages ->
                                      cfg.Pages
                                      |> Seq.tryFind (fun p -> p.Name = pi)
                                      |> Option.bind (fun pc -> Some pc.Title)
                                      |> Option.defaultWith (fun _ -> failwith "Missing page.")
                                  | IndexSectionType.CodeDocuments ->
                                      cfg.CodeDocuments.Sources
                                      |> Seq.tryFind (fun s -> s.Name = pi)
                                      |> Option.bind (fun pi -> Some pi.Name)
                                      |> Option.defaultWith (fun _ -> failwith "Missing code doc.") }: IndexPage)) }: IndexDefinition))

            ({ Name = cfg.Name
               Templates = templates
               Actions = actions @ createResourceActions cfg
               RootPath = path
               OutputRoot = outputRoot
               Indexes = indexes }: Context)
    *)

    let expandPath (root: string) (path: string) = Path.Combine(root, path)

    type ContextData =
        | Article of ArticlesData
        | FSharpCodeDocuments of FSharpCodeDocumentsData

    and ArticlesData =
        { SectionName: string
          Articles: ArticleData list
          Template: string }

    and ArticleData = { Title: string; Lines: string list }

    and FSharpCodeDocumentsData =
        { SectionName: string
          Modules: CodeDocuments.FSharp.Documentation.ModuleDocument list
          Template: string }

    type Context =
        { Name: string
          Templates: TemplateCache
          RootPath: string
          OutputRoot: string
          Configuration: PipelineConfiguration }

        member ctx.Run() =
            // Start by generating the overall index.

            // Extract

            let regexIgnore = "Internal"

            let outputPath =
                "C:\\ProjectData\\Peeps\\docs\\api"

            let (data, indexes) =
                ctx.Configuration.Sections
                |> List.ofSeq
                |> List.map (fun s ->
                    match ctx.Templates.Values.TryFind s.Template, s.Type with
                    | Some template, "articles" ->
                        // Generate article.
                        let articles =
                            s.Items
                            |> List.ofSeq
                            |> List.choose (fun p ->
                                loadLines (expandPath ctx.RootPath p)
                                |> Option.map (fun a -> ({ Title = a.[0]; Lines = a }: ArticleData)))

                        ({ SectionName = s.Title
                           Articles = articles
                           Template = s.Template }: ArticlesData)
                        |> ContextData.Article
                        |> fun r -> Some(r, Articles.Indexes.extract s.Title (articles |> List.map (fun a -> a.Title)))
                    | Some template, "fsharp_code_documents" ->
                        let modules =
                            CodeDocuments.FSharp.SourceExtractor.extractMultiple
                                (s.Items
                                 |> List.ofSeq
                                 |> List.map (fun p -> expandPath ctx.RootPath p))
                                regexIgnore
                            |> Map.toList
                            |> List.collect snd
                            |> CodeDocuments.FSharp.SourceExtractor.group

                        // Generate fsharp code doc.

                        // Extract
                        { SectionName = s.Title
                          Modules = modules
                          Template = s.Template }
                        |> ContextData.FSharpCodeDocuments
                        |> fun r ->
                            Some(
                                r,
                                CodeDocuments.FSharp.Indexes.extract (slugifyName s.Title) s.Title regexIgnore modules
                            )
                    | Some _, t ->
                        printfn $"Unknown section type `{t}`. Skipping."
                        None
                    | None, _ ->
                        printfn $"Template `{s.Template}` not found. Skipping."
                        None)
                |> List.choose id
                |> List.fold (fun (cds, is) (cd, i) -> cds @ [ cd ], is @ [ i ]) ([], [])

            data
            |> List.iter (function
                | ContextData.Article ad ->
                    match ctx.Templates.Values.TryFind ad.Template with
                    | Some t ->

                        ad.Articles
                        |> List.iter (fun a ->
                            let index =
                                Articles.Indexes.generate indexes

                            Articles.ArticleRenderer.run t a.Title Map.empty index a.Lines
                            |> fun r -> File.WriteAllText(Path.Combine(outputPath, $"{a.Title |> slugifyName}.html"), r))
                    | None -> ()
                | ContextData.FSharpCodeDocuments fsd ->
                    match ctx.Templates.Values.TryFind fsd.Template with
                    | Some t ->

                        fsd.Modules
                        |> List.iter (fun m ->
                            let index =
                                CodeDocuments.FSharp.Indexes.generate (slugifyName fsd.SectionName) indexes regexIgnore m

                            CodeDocuments.FSharp.Templating.generateModulePage t m.DisplayName index [] m
                            |> fun r -> File.WriteAllText(Path.Combine(outputPath, $"{m.Id}.html"), r))
                    | None -> ())

    and TemplateCache =
        { Values: Map<string, Mustache.Token list> }

        static member Empty = { Values = Map.empty }

        /// Will check for a template, if not found. It will attempt to load and parse the template.
        /// In this
        member tc.LoadAndAdd(key: string, path: string) =
            match tc.Values.TryFind key with
            | Some _ -> tc
            | None ->
                File.ReadAllText path
                |> Mustache.parse
                |> fun r -> { tc with Values = tc.Values.Add(key, r) }

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

(*
[<AutoOpen>]
module ImplV1 =

    open System
    open System.IO
    open System.Text.Json
    open System.Text.Json.Serialization
    open FDOM.Core.Parsing
    open FDOM.Rendering
    //open FXD.Documentation
    open Fluff.Core

    type Context =
        { Name: string
          Templates: TemplateCache
          Actions: Action list
          RootPath: string
          OutputRoot: string
          Indexes: IndexDefinition list }

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
                |> fun r -> { tc with Values = tc.Values.Add(path, r) }

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

    and IndexDefinition =
        { SectionName: string
          Type: IndexSectionType
          Pages: IndexPage list }

    and [<RequireQualifiedAccess>] IndexSectionType =
        | Pages
        | CodeDocuments

    and IndexPage = { Name: string; Title: string }

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
            (indexes: IndexDefinition list)
            =
            match templates.Values.TryFind action.Template with
            | Some template ->

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


                (*
                // Create the indexes
                let pageIndexes =
                    Html.getIndexes doc
                    |> List.map
                        (fun i ->
                            [ "index_slug", i |> slugifyName |> Mustache.Value.Scalar
                              "index_title", i |> Mustache.Value.Scalar ]
                            |> Map.ofList
                            |> Mustache.Value.Object)
                    |> Mustache.Value.Array
                *)

                // Merge page indexes with generate indexes.
                let indexesHtml =
                    indexes
                    |> List.map (fun i ->
                        let pagesIndexHtml =
                            i.Pages
                            |> List.map (fun pi ->
                                match pi.Name with
                                | _ when String.Equals(pi.Name, action.TitleSlug) ->
                                    // Generate the open index.
                                    let content =
                                        Html.getIndexes doc
                                        |> List.map (fun i -> $"""<a href="#{i |> slugifyName}">{i}</a>""")
                                        |> String.concat ""

                                    $"""<li class="open"><h3>{pi.Title}</h3>{content}</li>"""
                                | _ -> $"""<li><a href="./{pi.Name}.html">{pi.Title}</a></li>""")
                            |> String.concat ""

                        $"""<div class="index-section"><h2>{i.SectionName}</h2><ul>{pagesIndexHtml}</ul></div>""")
                    |> String.concat ""



                let values =
                    ({ Values =
                        [ "title", Mustache.Value.Scalar action.Title
                          "titleSlug", Mustache.Value.Scalar action.TitleSlug
                          "now", Mustache.Value.Scalar(DateTime.Now.ToString("dd MMMM yyyy HH:mm:ss"))
                          "index", Mustache.Value.Scalar indexesHtml ]
                        @ (action.Metadata
                           |> Map.toList
                           |> List.map (fun (k, v) -> k, Mustache.Value.Scalar v))
                        |> Map.ofList
                       Partials = Map.empty }: Mustache.Data)

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

        let createPageActions
            (args: Map<string, string>)
            (globalMetadata: Map<string, string>)
            (rootPath: string)
            (cfg: Configuration)
            (acc: Action list)
            =
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
                              TitleSlug = substituteValue args p.TitleSlug
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
                |> List.map (fun cs ->
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

            let globalMetadata =
                cfg.Metadata |> createMetadata argsV Map.empty

            let (actions, templates) =
                cfg.Directories
                |> List.ofSeq
                |> createDirectoryActions
                |> createPageActions argsV globalMetadata path cfg
                |> createCodeDocumentActions globalMetadata argsV path cfg

            let indexes =
                cfg.Indexes
                |> List.ofSeq
                |> List.map (fun i ->
                    let t =
                        match i.Type.ToLower() with
                        | "pages" -> IndexSectionType.Pages
                        | "codedocuments" -> IndexSectionType.CodeDocuments
                        | _ -> failwith $"Unknown index section type: `{i.SectionName}`"

                    ({ SectionName = i.SectionName
                       Type = t
                       Pages =
                         i.Pages
                         |> List.ofSeq
                         |> List.map (fun pi ->
                             ({ Name = pi
                                Title =
                                  match t with
                                  | IndexSectionType.Pages ->
                                      cfg.Pages
                                      |> Seq.tryFind (fun p -> p.Name = pi)
                                      |> Option.bind (fun pc -> Some pc.Title)
                                      |> Option.defaultWith (fun _ -> failwith "Missing page.")
                                  | IndexSectionType.CodeDocuments ->
                                      cfg.CodeDocuments.Sources
                                      |> Seq.tryFind (fun s -> s.Name = pi)
                                      |> Option.bind (fun pi -> Some pi.Name)
                                      |> Option.defaultWith (fun _ -> failwith "Missing code doc.") }: IndexPage)) }: IndexDefinition))

            ({ Name = cfg.Name
               Templates = templates
               Actions = actions @ createResourceActions cfg
               RootPath = path
               OutputRoot = outputRoot
               Indexes = indexes }: Context)

    let load (path: string) (outputRoot: string) (args: Map<string, string>) =
        File.ReadAllText <| Path.Combine(path, "fxd.json")
        |> JsonSerializer.Deserialize<Configuration>
        |> Internal.createContext path outputRoot args

    let run (ctx: Context) =

        // TODO create indexes.
        // 1. Add indexes to config
        // 2. Create "general" indexes (i.e. unexpanded)
        // 3. For each page, if indexed, replace

        ctx.Actions
        |> List.iter (fun a ->
            match a with
            | GeneratePage gpa ->
                Internal.generatePage
                    ctx.Templates
                    ctx.RootPath
                    (Path.Combine(ctx.OutputRoot, gpa.Output))
                    gpa
                    ctx.Indexes
                |> fun r -> printfn $"*** {r}"
            | GenerateCodeDocument gcda ->
                // Extract the source data.

                // TODO extract top level data, then dig down.
                // Not sure exactly how this will work.

                SourceExtractor.extract gcda.Source
                |> List.map (fun m ->
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
                            |> fun r -> File.WriteAllText(Path.Combine(ctx.OutputRoot, $"{cd.Id}.html"), r)
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
*)
