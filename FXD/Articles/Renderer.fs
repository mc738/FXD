namespace FXD.Articles

open System
open System.IO
open FDOM.Core.Common
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
        (additionalValues: Map<string, Mustache.Value>)
        (sectionTitle: string)
        (indexHtml: string)
        (doc: FDOM.Core.Common.DOM.Document)
        =
        let title =  doc.GetTitleText()

        let values =
            ({ Values =
                [ "section_title", Mustache.Value.Scalar sectionTitle
                  "title", Mustache.Value.Scalar title
                  "index", Mustache.Value.Scalar indexHtml ]
                |> Map.ofList
                |> concatMappedValues additionalValues
               Partials = Map.empty }: Mustache.Data)

        Html.renderFromParsedTemplate template values [] [] doc
