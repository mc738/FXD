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
            
            
    let generate (sectionId: string) (articleId: string) (articleTitle: string) (pageIndexes: (string * string) list) (sections: IndexSection list) =
        sections
        |> List.map (fun is ->
            match is.Id = sectionId with
            | true ->
                let r = pageIndexes |> List.map (fun (id, name) -> $"""<a href="#{id}">{name}</a>""") |> String.concat ""
                
                let generate _ =
                    $"""<li class="open"><h3>{articleTitle}</h3>{r}</li>"""
                is.ToHtmlWithReplacement(articleId, generate)
            | false -> is.ToHtml())
        |> String.concat ""

