namespace FXD

open System
open System.IO
open System.Reflection.Metadata
open FDOM.Core.Common
open FDOM.Core.Common
open FDOM.Core.Dsl.Article
open FDOM.Rendering
open FXD.Documentation
open FXD.XmlDocExtractor

module Documents =

    type CompiledMember =
        { Name: string
          Member: Member
          Blocks: DOM.BlockContent list
          Children: CompiledMember list option }

    let createSummary _ = ()

    let (>+>) a b = List.concat [ a; b ]

    let (>?>) a b =
        match b with
        | Some l -> a >+> l
        | None -> a

    let (<++) a b = [ b ] @ a

    let handleXmlDoc doc =
        match doc with
        | Some xdoc ->
            let summary =
                match xdoc.Summary with
                | Some s -> [ p [ text s ] ] |> Some
                | None -> None

            let returns =
                match xdoc.Returns with
                | Some s -> [ h3 [ text "Returns" ]; p [ text s ] ] |> Some
                | None -> None

            let examples =
                xdoc.Examples
                |> List.map (fun e -> codeBlock [ text e ])
                <++ h3 [ text "Examples" ]

            summary, examples, returns
        | None -> None, [], None

    let handleFunction (fd: FunctionDocument) =
        let (summary, examples, returns) = handleXmlDoc fd.XmlDocument

        let signature =
            [ h3 [ text "Signature" ]
              codeBlock [ text fd.Signature ] ]

        let parameters =
            fd.Parameters
            |> List.collect
                (fun param ->
                    [ yield p [ text "Name: "; text param.Name ]
                      yield p [ text "Type: "; text param.Type ]
                      yield p [ text (param.Document |> Option.defaultValue "") ] ])
            <++ h3 [ text "Parameters" ]

        [ h2 [ text fd.DisplayName ] ] >?> summary
        >+> signature
        >+> parameters
        >?> returns
        >+> examples

    let handleUnion (ud: UnionDocument) =
        let (summary, examples, returns) = handleXmlDoc ud.XmlDocument

        let cases =
            ud.Members
            |> List.collect
                (fun um ->
                    let name = [ h4 [ text um.Name ] ]
                    let (summary, _, _) = handleXmlDoc ud.XmlDocument
                    name >?> summary)
            <++ h3 [ text "Cases" ]

        [ h2 [ text ud.DisplayName ] ] >?> summary
        >+> cases
        >?> returns
        >+> examples

    let handleRecord (ud: RecordDocument) =
        let (summary, examples, returns) = handleXmlDoc ud.XmlDocument

        let fields =
            ud.Fields
            |> List.collect
                (fun rf ->
                    let name = [ h4 [ text rf.Name ] ]
                    let (summary, _, _) = handleXmlDoc ud.XmlDocument

                    name
                    >+> [ p [ text "Type: " ]
                          p [ text rf.Type ] ]
                    >?> summary)
            <++ h3 [ text "Cases" ]

        [ h2 [ text ud.DisplayName ] ] >?> summary
        >+> fields
        >?> returns
        >+> examples


    let handleMethod (md: MethodDocument) =
        let (summary, examples, returns) = handleXmlDoc md.XmlDocument

        let signature =
            [ h3 [ text "Signature" ]
              codeBlock [ text md.Signature ] ]

        let parameters =
            md.Parameters
            |> List.collect
                (fun param ->
                    [ yield p [ text "Name: "; text param.Name ]
                      yield p [ text "Type: "; text param.Type ]
                      yield p [ text (param.Document |> Option.defaultValue "") ] ])
            <++ h3 [ text "Parameters" ]

        [ h2 [ text md.Name ] ] >?> summary
        >+> signature
        >+> parameters
        >?> returns
        >+> examples


    let rec handleProperties (pd: PropertyDocument) =
        let (summary, examples, returns) = handleXmlDoc pd.XmlDocument

        let pType =
            [ h3 [ text "Type" ]
              codeBlock [ text pd.Type ] ]

        [ h2 [ text pd.Name ] ] >?> summary >+> pType
        >?> returns
        >+> examples

    let handleClass (cd: ClassDocument) =
        let (summary, examples, returns) = handleXmlDoc cd.XmlDocument

        let properties =
            cd.Properties
            |> List.map handleProperties
            |> List.concat
            <++ h3 [ text "Properties" ]

        let methods =
            cd.Methods |> List.map handleMethod |> List.concat
            <++ h3 [ text "Methods" ]

        [ h2 [ text cd.DisplayName ] ] >?> summary
        >+> properties
        >+> methods
        >?> returns
        >+> examples

    let handleModule (md: ModuleDocument) =
        let (summary, examples, returns) = handleXmlDoc md.XmlDocument

        let members =
            md.Members
            |> List.choose
                (fun m ->
                    match m with
                    | Member.Function fd -> handleFunction fd |> Some
                    | _ -> None)
            |> List.concat

        [ h1 [ text md.DisplayName ] ] >?> summary
        >+> members

    let create (resources: DOM.Resource list) (members: Member list) =
        members
        |> List.map
            (fun m ->
                match m with
                | Member.Function fd ->
                    { Name = fd.DisplayName
                      Member = m
                      Blocks = handleFunction fd
                      Children = None }

                | Member.Union ud ->
                    { Name = ud.DisplayName
                      Member = m
                      Blocks = handleUnion ud
                      Children = None }

                | Member.Record rd ->
                    { Name = rd.DisplayName
                      Member = m
                      Blocks = handleRecord rd
                      Children = None }
                | Member.Class cd ->
                    { Name = cd.DisplayName
                      Member = m
                      Blocks = handleClass cd
                      Children = None }
                | Member.Module md ->
                    { Name = md.DisplayName
                      Member = m
                      Blocks = handleModule md
                      Children = None }
                | Member.Namespace ns ->
                    { Name = ""
                      Member = m
                      Blocks = []
                      Children = None })
        |> List.map (fun r -> section r.Name None DOM.Style.Default r.Blocks)
        |> fun r -> document "test" None DOM.Style.Default r resources

    let html path (document: DOM.Document) =
        let stylesheets =
            document.Resources
            |> List.filter (fun r -> r.Type = "stylesheet")
            |> List.map (fun r -> $"{document.SnakeCaseName}/{r.VirtualPath}")

        let scripts =
            document.Resources
            |> List.filter (fun r -> r.Type = "script")
            |> List.map (fun r -> $"{document.SnakeCaseName}/{r.VirtualPath}")

        let layout: Html.Layout =
            { Head = "<section id=\"sidebar\"><small>Main</small></section><main><small>Main</small>"
              Foot = "</main>" }

        let html =
            Html.render layout stylesheets scripts document

        File.WriteAllText(path, html)

    let htmlFromTemplate templatePath data path (document: DOM.Document) =
        let stylesheets =
            document.Resources
            |> List.filter (fun r -> r.Type = "stylesheet")
            |> List.map (fun r -> $"{document.SnakeCaseName}/{r.VirtualPath}")

        let scripts =
            document.Resources
            |> List.filter (fun r -> r.Type = "script")
            |> List.map (fun r -> $"{document.SnakeCaseName}/{r.VirtualPath}")

        let html =
            Html.renderFromTemplate templatePath data stylesheets scripts document

        File.WriteAllText(path, html)

    let pdf path (stylePath: string) (document: DOM.Document) = Pdf.render path stylePath document

    let render (renderers: (DOM.Document -> unit) list) (document: DOM.Document) =
        renderers
        |> List.map (fun fn -> fn document)
        |> ignore

//let qh = QueryHandler.Create($"/home/max/Data/FDOM_Tests/blob_store/{DateTime.Now:yyyyMMddHHmmss}.db")

//let ds = DocumentStore.Create($"/home/max/Data/FDOM_Tests/blob_store/{DateTime.Now:yyyyMMddHHmmss}.db")

//let rendererDocs: DOM.RenderedDocument list = [
//    { Path = renderedDocPath; VirtualPath = "index.html" }
//]
