namespace FXD

open System
open System.IO
open System.Reflection.Metadata
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open System.Xml.Linq
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Xml

[<AutoOpen>]
module Common =
    let listMap<'In, 'Out> (fn: 'In -> 'Out) (s: seq<'In>) = s |> List.ofSeq |> List.map fn

    let slugifyName (name: string) =
        name
        |> Seq.fold
            (fun acc c ->
                match c with
                | _ when Char.IsLetterOrDigit c -> acc @ [ Char.ToLower c ]
                | _ when c = ' ' -> acc @ [ '_' ]
                | _ when c = '-' -> acc @ [ c ]
                | _ when c = '.' -> acc @ [ '-' ]
                | _ -> acc)
            []
        |> fun c -> String.Join("", c)

    let tryLoadLines (path: string) =
        try
            File.ReadAllLines path |> List.ofArray |> Ok
        with
        | exn -> Error exn
        
    let loadLines (path: string) =
        match tryLoadLines path with
        | Ok l -> Some l
        | Error _ -> None
    
    type IndexSection =
        { Id: string
          Title: string
          Items: IndexItem list }

        member is.ToHtml() =
            let indexes =
                is.Items
                |> List.map (fun ie -> ie.ToHtml())
                |> String.concat ""

            $"""<div class="index-section"><h2>{is.Title}</h2><ul>{indexes}</ul></div>"""

        member is.ToHtmlWithReplacement(id: string, fn: unit -> string) =
            let indexes =
                is.Items
                |> List.map (fun ie ->
                    match ie.Id = id with
                    | true -> fn ()
                    | false -> ie.ToHtml())
                |> String.concat ""

            $"""<div class="index-section"><h2>{is.Title}</h2><ul>{indexes}</ul></div>"""


    and IndexItem =
        { Id: string
          Name: string
          Link: string }

        member ie.ToHtml() =
            $"""<li><a href="{ie.Link}.html">{ie.Name}</a></li>"""
