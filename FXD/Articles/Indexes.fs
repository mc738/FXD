namespace FXD.Articles

open FXD


module Indexes =


    let extract (sectionTitle: string) (titles: string list) =

        titles
        |> List.map (fun t ->
            ({ Id = slugifyName t
               Name = t
               Link = $"./{slugifyName t}" }: IndexItem))
        |> fun r ->
            ({ Id = slugifyName sectionTitle
               Title = sectionTitle
               Items = r }: IndexSection)
            
            
    let generate (sections: IndexSection list) =
        sections
        |> List.map (fun is -> is.ToHtml())
        |> String.concat ""

