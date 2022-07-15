namespace FXD.CodeDocuments.FSharp

module XmlDocExtractor =
    
    open System.IO
    open System.Xml.Linq
    open FSharp.Compiler.Symbols
    open FXD
    
    type XmlParameter = { Name: string; Description: string }

    type XmlDocumentMember =
        { Name: string
          Summary: string option
          Parameters: XmlParameter list
          Returns: string option
          Examples: string list }

    [<AutoOpen>]
    module Helpers =

        let xName (name: string) = XName.op_Implicit name

        let getElement (name: string) (el: XElement) =
            let found = el.Element(xName name)

            match found <> null with
            | true -> Some found
            | false -> None

        let getAttribute (name: string) (el: XElement) =
            let found = el.Attribute(xName name)

            match found <> null with
            | true -> Some found
            | false -> None

    let load (path: string) =
        File.ReadAllText path
        |> XDocument.Parse
        |> fun xdoc -> xdoc.Root

    let extractMember (el: XElement) =
        { Name =
            el
            |> getAttribute "name"
            |> Option.map (fun at -> at.Value)
            |> Option.defaultValue ""
          Summary =
            el
            |> getElement "summary"
            |> Option.map (fun el -> el.Value)
          Parameters =
            el.Elements(xName "param")
            |> listMap (fun pel ->
                { Name =
                    pel
                    |> getAttribute "name"
                    |> Option.map (fun at -> at.Value)
                    |> Option.defaultValue ""
                  Description = pel.Value })
          Returns =
            el
            |> getElement "returns"
            |> Option.map (fun el -> el.Value)
          Examples =
            el.Elements(xName "example")
            |> listMap (fun eel -> eel.Value) }

    let extract path =
        let root = load path

        match root |> getElement "members" with
        | Some el ->
            el.Elements(xName "member")
            |> listMap (fun mel -> extractMember mel)
            |> List.map (fun m -> m.Name, m)
            |> Map.ofList
        | None -> failwith "Error"

    let extractXmlDoc (name: string) (doc: FSharpXmlDoc) =
        match doc with
        | FSharpXmlDoc.FromXmlText txt ->
            match txt.UnprocessedLines.Length > 0 with
            | true ->
                try
                    let xml =
                        txt.UnprocessedLines
                        |> String.concat ""
                        |> fun b -> $"<member name=\"{name}\">{b}</member>"

                    extractMember (XElement.Parse(xml)) |> Some
                with
                | _ -> None
            | false -> None
        | FSharpXmlDoc.FromXmlFile _ -> None
        | FSharpXmlDoc.None _ -> None
