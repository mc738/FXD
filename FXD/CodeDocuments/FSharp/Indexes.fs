namespace FXD.CodeDocuments.FSharp

open FXD
open FXD.CodeDocuments.FSharp.Documentation

module Indexes =

    open System.Text.RegularExpressions
    open FXD
    open Documentation

    let generateIndex
        (indexEntries: Map<string, string>)
        (sectionId: string)
        (name: string)
        (regexIgnore: string)
        (members: Member list)
        =
        indexEntries
        |> Map.toList
        |> List.map (fun (id, v) ->
            match sectionId = id with
            | true ->
                let contents =
                    members
                    |> List.fold
                        (fun (a, c, r, u, f, m) cm ->
                            match cm.MatchName(regexIgnore), cm with
                            | false, Member.Abbreviation _ -> a @ [ cm ], c, r, u, f, m
                            | false, Member.Class _ -> a, c @ [ cm ], r, u, f, m
                            | false, Member.Record _ -> a, c, r @ [ cm ], u, f, m
                            | false, Member.Union _ -> a, c, r, u @ [ cm ], f, m
                            | false, Member.Function _ -> a, c, r, u, f @ [ cm ], m
                            | false, Member.Module _ -> a, c, r, u, f, m @ [ cm ]
                            | true, _ -> a, c, r, u, f, m
                            | _, Member.Namespace _ -> a, c, r, u, f, m)
                        ([], [], [], [], [], [])
                    |> fun (a, c, r, u, f, m) -> [ a; c; r; u; f; m ] |> List.concat
                    |> List.map (fun m -> $"""<a href="#{m.GetId()}">{m.GetDisplayName()}</a>""")
                    |> String.concat ""

                $"""<li class="open"><h3>{name}</h3>{contents}</li>"""
            | false -> v)

    let extract (sectionId: string) (title: string) (regexIgnore: string) (modules: ModuleDocument list) =

        modules
        |> List.map (fun m ->
            match Regex.IsMatch(m.FullName, regexIgnore) with
            | true -> None
            | false ->
                Some(
                    { Id = m.Id
                      Name = m.DisplayName
                      Link = $"./{m.Id}" }: IndexItem
                ))
        |> List.choose id
        |> fun r ->
            ({ Id = sectionId
               Title = title
               Items = r }: IndexSection)

    let generate (sectionId: string) (regexIgnore: string) (m: ModuleDocument) (sections: IndexSection list) =
        sections
        |> List.map (fun is ->
            match is.Id = sectionId with
            | true ->
                let generate _ =
                    m.Members
                    |> List.fold
                        (fun (a, c, r, u, f, m) cm ->
                            match cm.MatchName(regexIgnore), cm with
                            | false, Member.Abbreviation _ -> a @ [ cm ], c, r, u, f, m
                            | false, Member.Class _ -> a, c @ [ cm ], r, u, f, m
                            | false, Member.Record _ -> a, c, r @ [ cm ], u, f, m
                            | false, Member.Union _ -> a, c, r, u @ [ cm ], f, m
                            | false, Member.Function _ -> a, c, r, u, f @ [ cm ], m
                            | false, Member.Module _ -> a, c, r, u, f, m @ [ cm ]
                            | true, _ -> a, c, r, u, f, m
                            | _, Member.Namespace _ -> a, c, r, u, f, m)
                        ([], [], [], [], [], [])
                    |> fun (a, c, r, u, f, m) -> [ a; c; r; u; f; m ] |> List.concat
                    |> List.map (fun m -> $"""<a href="#{m.GetId()}">{m.GetDisplayName()}</a>""")
                    |> String.concat ""
                    |> fun r ->  $"""<li class="open"><h3>{m.DisplayName}</h3>{r}</li>"""
                is.ToHtmlWithReplacement(m.Id, generate)
            | false -> is.ToHtml())
        |> String.concat ""