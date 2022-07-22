namespace FXD.Articles

open FDOM.Core.Common

module Templating =

    open FDOM.Core.Common
    open FDOM.Core.Parsing
    open FXD

    let extract (source: string list) =

        let blocks =
            Parser.ParseLines(source).CreateBlockContent()
            // Index headers.
            |> List.map (function
                | DOM.BlockContent.Header h when h.Level = DOM.HeaderLevel.H1 ->
                    DOM.BlockContent.Header { h with Indexed = false }
                | DOM.BlockContent.Header h when h.Level = DOM.HeaderLevel.H2 ->
                    DOM.BlockContent.Header { h with Indexed = true }
                | b -> b)

        let title =
            blocks
            |> List.tryHead
            |> Option.bind (function
                | DOM.BlockContent.Header h -> Some h
                | _ -> None)

        match title with
        | Some title ->

            ({ Style = FDOM.Core.Common.DOM.Style.Default
               Name = title.GetRawText() |> slugifyName
               Title = Some title
               Sections =
                 [ { Style = FDOM.Core.Common.DOM.Style.Default
                     Title = None
                     Name = "Section 1"
                     Content = blocks } ]
               Resources =
                 [ { Name = "main_css"
                     Path = "/home/max/Data/FDOM_Tests/css/style.css"
                     VirtualPath = "css/style.css"
                     Type = "stylesheet" } ] }: DOM.Document)
            |> Ok
        | None -> Error "Missing article title."
