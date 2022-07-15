namespace FXD.Articles

open FXD


module Indexes =


    let extract (sectionTitle: string) (documents: (FDOM.Core.Common.DOM.Document) list) =

        documents
        |> List.map (fun d ->
            ({ Id = d.Name
               Name = d.GetTitleText()
               Link = $"./{d.Name}" }: IndexItem))
        |> fun r ->
            ({ Id = slugifyName sectionTitle
               Title = sectionTitle
               Items = r }: IndexSection)
            
            
    let generate (sectionId: string) (articleId: string) (articleTitle: string) (sections: IndexSection list) =
        sections
        |> List.map (fun is ->
            match is.Id = sectionId with
            | true ->
                let generate _ =
                    $"""<li class="open"><h3>{articleTitle}</h3></li>"""
                is.ToHtmlWithReplacement(articleId, generate)
            | false -> is.ToHtml())
        |> String.concat ""

