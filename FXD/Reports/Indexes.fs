namespace FXD.Reports

module Indexes =

    open FXD
    
    let extract (sectionTitle: string) (items: (string * string) list) =
        
        items
        |> List.map (fun (id, name) ->
            ({ Id = id
               Name = name
               Link = $"./{id}" }: IndexItem))
        |> fun r ->
            ({ Id = slugifyName sectionTitle
               Title = sectionTitle
               Items = r }: IndexSection)
            
    let generate (sectionId: string) (reportId: string) (reportTitle: string) (sections: IndexSection list) =
        sections
        |> List.map (fun is ->
            match is.Id = sectionId with
            | true ->
                //let r = pageIndexes |> List.map (fun (id, name) -> $"""<a href="#{id}">{name}</a>""") |> String.concat ""
                
                let generate _ =
                    $"""<li class="open"><h3>{reportTitle}</h3></li>"""
                is.ToHtmlWithReplacement(reportTitle, generate)
            | false -> is.ToHtml())
        |> String.concat ""