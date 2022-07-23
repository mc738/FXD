namespace FXD.Pipelines

module Context =

    open System.IO
    open Fluff.Core
    open FXD
    open FXD.Pipelines.Configuration

    [<AutoOpen>]
    module Utils =

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
            |> toMappedValues
            //|> Map.fold (fun (acc: Map<string, Mustache.Value>) k v -> acc.Add(k, Mustache.Value.Scalar v)) Map.empty
            |> concatMappedValues (globalMetadata |> toMappedValues)

    type TemplateCache =
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

    type ContextData =
        | Article of ArticlesData
        | FSharpCodeDocuments of FSharpCodeDocumentsData
        | Reports of ReportsData

    and ArticlesData =
        { SectionName: string
          Articles: ArticleData list
          Template: string }

    and ArticleData =
        { Document: FDOM.Core.Common.DOM.Document }

    and FSharpCodeDocumentsData =
        { SectionName: string
          Modules: FXD.CodeDocuments.FSharp.Documentation.ModuleDocument list
          RegexIgnore: string
          Template: string }

    and ReportsData =
        { SectionTitle: string
          Reports: ReportType list }

    and ReportType = ProjectReport of ProjectReportData

    and ProjectReportData =
        { SectionName: string
          Data: Mustache.Value list
          Template: string }
