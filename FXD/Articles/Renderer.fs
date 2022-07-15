namespace FXD.Articles

open System
open System.IO
open FDOM.Core.Parsing
open FDOM.Rendering
open FXD
open Fluff.Core

[<RequireQualifiedAccess>]
module ArticleRenderer =

    open FXD

    let load (path: string) = File.ReadAllLines(path) |> List.ofArray

    let run
        (template: Mustache.Token list)
        (title: string)
        (metadata: Map<string, string>)
        (indexHtml: string)
        (source: string list)
        =

        let blocks =
            Parser.ParseLines(source).CreateBlockContent()

        let doc: FDOM.Core.Common.DOM.Document =
            { Style = FDOM.Core.Common.DOM.Style.Default
              Name = title
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

        let values =
            ({ Values =
                [ "title", Mustache.Value.Scalar title
                  "titleSlug", Mustache.Value.Scalar <| slugifyName title
                  "now", Mustache.Value.Scalar(DateTime.Now.ToString("dd MMMM yyyy HH:mm:ss"))
                  "index", Mustache.Value.Scalar indexHtml ]
                @ (metadata
                   |> Map.toList
                   |> List.map (fun (k, v) -> k, Mustache.Value.Scalar v))
                |> Map.ofList
               Partials = Map.empty }: Mustache.Data)

        Html.renderFromParsedTemplate template values [] [] doc
