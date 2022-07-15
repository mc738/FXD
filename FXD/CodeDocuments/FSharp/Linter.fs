namespace FXD.CodeDocuments.FSharp

[<RequireQualifiedAccess>]
module Linter =

    open Documentation
    open XmlDocExtractor

    type LintingError =
        | MissingXmlDocMember of string
        | MissingXmlDocSummary of string
        | MissingXmlDocParameter of string
        | MissingXmlDocReturn of string
        | MissingXmlDocExample of string
        | UnknownType of string

    type LintSettings =
        { SummaryRequired: bool
          ParametersRequired: bool
          ReturnsRequired: bool
          ExamplesRequired: bool }

        static member Strict =
            { SummaryRequired = true
              ParametersRequired = true
              ReturnsRequired = true
              ExamplesRequired = true }

    type LintingResult =
        { Members: Member list
          LintErrors: LintingError list }

    let lintXmlDocument (settings: LintSettings) (doc: XmlDocumentMember) =
        Some [ match doc.Summary with
               | Some _ -> ()
               | None ->
                   if settings.SummaryRequired then
                       yield LintingError.MissingXmlDocSummary doc.Name

               match doc.Parameters.Length > 0 with
               | true -> ()
               | false ->
                   if settings.ParametersRequired then
                       yield LintingError.MissingXmlDocParameter doc.Name

               match doc.Returns with
               | Some _ -> ()
               | None ->
                   if settings.ReturnsRequired then
                       yield LintingError.MissingXmlDocReturn doc.Name

               match doc.Examples.Length > 0 with
               | true -> ()
               | false ->
                   if settings.ExamplesRequired then
                       yield LintingError.MissingXmlDocExample doc.Name ]

    let run (errorsHandler: Member list -> LintingError list -> Member list) (members: Member list) =

        let rec handler (m: Member) =
            match m with
            | Member.Function fd ->
                fd.XmlDocument
                |> Option.bind (lintXmlDocument LintSettings.Strict)
                |> Option.defaultValue [ MissingXmlDocMember fd.DisplayName ]

            | Member.Union ud ->
                let r1 =
                    ud.XmlDocument
                    |> Option.bind (lintXmlDocument LintSettings.Strict)
                    |> Option.defaultValue [ MissingXmlDocMember ud.DisplayName ]

                let r2 =
                    ud.Members
                    |> List.map (fun um ->
                        um.XmlDocument
                        |> Option.bind (lintXmlDocument LintSettings.Strict)
                        |> Option.defaultValue [ MissingXmlDocMember um.Name ])
                    |> List.concat

                r1 @ r2
            | Member.Record rd ->
                let r1 =
                    rd.XmlDocument
                    |> Option.bind (lintXmlDocument LintSettings.Strict)
                    |> Option.defaultValue [ MissingXmlDocMember rd.DisplayName ]

                let r2 =
                    rd.Fields
                    |> List.map (fun rf ->
                        rf.XmlDocument
                        |> Option.bind (lintXmlDocument LintSettings.Strict)
                        |> Option.defaultValue [ MissingXmlDocMember rf.Name ])
                    |> List.concat

                r1 @ r2
            | Member.Class cd ->
                let r1 =
                    cd.XmlDocument
                    |> Option.bind (lintXmlDocument LintSettings.Strict)
                    |> Option.defaultValue [ MissingXmlDocMember cd.DisplayName ]

                let r2 = []

                r1 @ r2
            | Member.Module md ->
                let r1 =
                    md.XmlDocument
                    |> Option.bind (lintXmlDocument LintSettings.Strict)
                    |> Option.defaultValue [ MissingXmlDocMember md.DisplayName ]

                let r2 =
                    md.Members |> List.map handler |> List.concat

                r1 @ r2
            | Member.Namespace nd -> nd.Members |> List.map handler |> List.concat
            | Member.Abbreviation ad -> []

        errorsHandler members (members |> List.map handler |> List.concat)
