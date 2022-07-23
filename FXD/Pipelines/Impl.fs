namespace FXD.Pipelines

open System
open System.IO
open FDOM.Core.Common
open FXD
open FXD.Pipelines.Configuration
open FXD.Pipelines.Context
open Fluff.Core
open Context

module Articles =

    let extract (rootPath: string) (section: SectionItem) =
        let articles =
            section.Items
            |> List.ofSeq
            |> List.choose (fun p ->
                loadLines (expandPath rootPath p)
                |> Option.bind (fun a ->
                    match Articles.Templating.extract a with
                    | Ok d -> Some({ Document = d })
                    | Error e ->
                        printfn $"Error loading article. Error: `{e}`"
                        None))

        ({ SectionName = section.Title
           Articles = articles
           Template = section.Template }: ArticlesData)
        |> ContextData.Article
        |> fun r -> Some(r, Articles.Indexes.extract section.Title (articles |> List.map (fun a -> a.Document)))

    let render
        (outputPath: string)
        (template: Mustache.Token list)
        (additionValues: Map<string, Mustache.Value>)
        (indexes: IndexSection list)
        (data: ArticlesData)
        =
        data.Articles
        |> List.iter (fun a ->
            let pageIndexes =
                a.Document.GetIndexes(DOM.InlineContent.GetRawText)
                |> List.map (fun s -> slugifyName s, s)

            let index =
                Articles.Indexes.generate
                    (slugifyName data.SectionName)
                    a.Document.Name
                    (a.Document.GetTitleText())
                    pageIndexes
                    indexes

            Articles.ArticleRenderer.run template additionValues data.SectionName index a.Document
            |> fun r -> File.WriteAllText(Path.Combine(outputPath, $"{a.Document.Name |> slugifyName}.html"), r))

        ()

module FSharpCodeDocuments =

    let extract (rootPath: string) (section: SectionItem) =
        let modules =
            CodeDocuments.FSharp.SourceExtractor.extractMultiple
                rootPath
                (section.Items
                 |> List.ofSeq
                 |> List.map (fun p -> expandPath rootPath p))
                section.RegexIgnore
            |> Map.toList
            |> List.collect snd
            |> CodeDocuments.FSharp.SourceExtractor.group

        // Generate fsharp code doc.

        // Extract
        { SectionName = section.Title
          Modules = modules
          RegexIgnore = section.RegexIgnore
          Template = section.Template }
        |> ContextData.FSharpCodeDocuments
        |> fun r ->
            Some(
                r,
                CodeDocuments.FSharp.Indexes.extract
                    (slugifyName section.Title)
                    section.Title
                    section.RegexIgnore
                    modules
            )

    let render
        (outputPath: string)
        (template: Mustache.Token list)
        (additionValues: Map<string, Mustache.Value>)
        (sectionTitle: string)
        (version: string)
        (indexes: IndexSection list)
        (data: FSharpCodeDocumentsData)
        =
        data.Modules
        |> List.iter (fun m ->
            let index =
                CodeDocuments.FSharp.Indexes.generate (slugifyName data.SectionName) data.RegexIgnore m indexes

            let p = m.Path.Replace('\\', '/')
            let sourceLink = $"/blob/v.{version}/{p}"

            CodeDocuments.FSharp.Templating.generateModulePage
                template
                m.DisplayName
                sectionTitle
                additionValues
                sourceLink
                index
                m
            |> fun r -> File.WriteAllText(Path.Combine(outputPath, $"{m.Id}.html"), r))

module Reports =

    let extract (rootPath: string) (section: SectionItem) =

        let pages =
            section.Items
            |> List.ofSeq
            |> List.choose (function
                | "project_report" -> Some("project_report", "Project report")
                | _ -> None)

        //let reports =
        section.Items
        |> List.ofSeq
        |> List.choose (fun s ->
            match s with
            | "project_report" ->
                ({ SectionName = "Project report"
                   Data = Reports.ProjectReport.extract rootPath
                   Template = "project_report" }: ProjectReportData)
                |> ReportType.ProjectReport
                |> Some
            | _ -> None)
        |> fun r ->
            ({ SectionTitle = section.Title
               Reports = r }: ReportsData)
            |> ContextData.Reports
            |> fun r -> Some(r, Reports.Indexes.extract section.Title pages)

    let renderProjectReport
        (outputPath: string)
        (template: Mustache.Token list)
        (additionalValues: Map<string, Mustache.Value>)
        (indexes: IndexSection list)
        (data: Mustache.Value list)
        =
        let indexHtml = Reports.Indexes.generate "reports" "project_reports" "Projects report" indexes

        ({ Values =
            [ "section_title", Mustache.Value.Scalar "Reports"
              "title", Mustache.Value.Scalar "Project report"
              "projects", Mustache.Array data
              "index", Mustache.Value.Scalar indexHtml ]
            |> Map.ofList
            |> concatMappedValues additionalValues
           Partials = Map.empty }: Mustache.Data)
        |> fun d -> Mustache.replace d true template
        |> fun r -> File.WriteAllText(Path.Combine(outputPath, $"project_report.html"), r)
           
type DocumentPipeline =
    { Name: string
      Templates: TemplateCache
      RootPath: string
      OutputRoot: string
      GlobalMetaData: Map<string, string>
      DocumentTitle: string
      Version: string
      Configuration: PipelineConfiguration }

    member ctx.Run() =
        let globalMetaData =
            [ "project_name", ctx.Name
              "now", DateTime.Now.ToString("dd MMMM yyyy 'at' HH:mm:ss")
              "doc_title", ctx.DocumentTitle
              "version", ctx.Version ]
            |> Map.ofList
            |> concatStringMap ctx.GlobalMetaData

        let outputPath =
            Path.Combine(ctx.OutputRoot, slugifyName ctx.Version)

        if Directory.Exists outputPath |> not then
            Directory.CreateDirectory outputPath |> ignore

        let args =
            ctx.Configuration.Args
            |> List.ofSeq
            |> List.map (fun a -> a.Name, a.Default)
            |> Map.ofList

        let additionValues =
            createMetadata args globalMetaData ctx.Configuration.Metadata

        let (data, indexes) =
            ctx.Configuration.Sections
            |> List.ofSeq
            |> List.map (fun s ->
                match ctx.Templates.Values.TryFind s.Template, s.Type with
                | Some template, "articles" -> Articles.extract ctx.RootPath s
                | Some template, "fsharp_code_documents" -> FSharpCodeDocuments.extract ctx.RootPath s
                | _, "reports" -> Reports.extract ctx.RootPath s
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
                | Some t -> Articles.render outputPath t additionValues indexes ad
                | None -> ()
            | ContextData.FSharpCodeDocuments fsd ->
                match ctx.Templates.Values.TryFind fsd.Template with
                | Some t ->
                    FSharpCodeDocuments.render outputPath t additionValues fsd.SectionName ctx.Version indexes fsd
                | None -> ()
            | ContextData.Reports rs ->
                rs.Reports
                |> List.iter (fun r ->
                    match r with
                    | ReportType.ProjectReport prd ->
                        match ctx.Templates.Values.TryFind prd.Template with
                        | Some t -> Reports.renderProjectReport outputPath t additionValues indexes prd.Data
                        | None -> ()))
