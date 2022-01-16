// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Reflection.Metadata
open FXD
open FXD
open FXD.Documentation
open Fluff.Core
open FXD.XmlDocExtractor

// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

let passThru (members: Member list) (errors: Linter.LintingError list) =
    Console.ForegroundColor <- ConsoleColor.Magenta

    errors
    |> List.map (fun e -> printfn $"{e}")
    |> ignore

    Console.ResetColor()
    members

module Templating =

    let slugifyName (name: string) =
        name
        |> Seq.fold (fun acc c ->
            match c with
            | _ when Char.IsLetterOrDigit c -> Char.ToLower c
            ()) ()
        
        ()
    
    let handleXmlDoc doc =
        match doc with
        | Some xdoc ->
            let summary =
                match xdoc.Summary with
                | Some s -> Some s
                | None -> None

            let returns =
                match xdoc.Returns with
                | Some s -> Some s
                | None -> None

            let examples = xdoc.Examples

            summary, examples, returns
        | None -> None, [], None


    let createClassData (cd: ClassDocument) =
        let (summary, examples, returns) = handleXmlDoc cd.XmlDocument

        [ "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "name", Mustache.Value.Scalar cd.DisplayName
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "properties",
          Mustache.Value.Array(
              cd.Properties
              |> List.map
                  (fun p ->
                      let (pSummary, pExamples, pReturns) = handleXmlDoc p.XmlDocument

                      [ "className", Mustache.Scalar cd.DisplayName
                        "name", Mustache.Value.Scalar p.Name
                        "type", Mustache.Value.Scalar p.Type
                        "summary", Mustache.Value.Scalar(pSummary |> Option.defaultValue "")
                        "returns", Mustache.Value.Scalar(pReturns |> Option.defaultValue "")
                        "examples", Mustache.Value.Array(pExamples |> List.map Mustache.Value.Scalar) ]
                      |> Map.ofList
                      |> Mustache.Object)
          )
          "methods",
          Mustache.Value.Array(
              cd.Methods
              |> List.map
                  (fun m ->
                      let (mSummary, mExamples, mReturns) = handleXmlDoc m.XmlDocument

                      [ "className", Mustache.Scalar ""
                        "name", Mustache.Value.Scalar m.Name
                        "signature", Mustache.Value.Scalar m.Signature
                        "parameters",
                        m.Parameters
                        |> List.map
                            (fun p ->
                                [ "name", Mustache.Value.Scalar p.Name
                                  "document", Mustache.Value.Scalar(p.Document |> Option.defaultValue "")
                                  "type", Mustache.Value.Scalar p.Type ]
                                |> Map.ofList
                                |> Mustache.Value.Object)
                        |> Mustache.Value.Array
                        "summary", Mustache.Value.Scalar(mSummary |> Option.defaultValue "")
                        "returns", Mustache.Value.Scalar(mReturns |> Option.defaultValue "")
                        "examples", Mustache.Value.Array(mExamples |> List.map Mustache.Value.Scalar) ]
                      |> Map.ofList
                      |> Mustache.Object)
          ) ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createFunctionData (fd: FunctionDocument) =
        let (summary, examples, returns) = handleXmlDoc fd.XmlDocument

        [ "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "signature", Mustache.Value.Scalar fd.Signature
          "name", Mustache.Value.Scalar fd.DisplayName
          "parameters",
          Mustache.Value.Array(
              fd.Parameters
              |> List.map
                  (fun p ->
                      [ "name", Mustache.Value.Scalar p.Name
                        "document", Mustache.Value.Scalar(p.Document |> Option.defaultValue "")
                        "type", Mustache.Value.Scalar p.Type ]
                      |> Map.ofList
                      |> Mustache.Object)
          ) ]
        |> Map.ofList
        |> Mustache.Value.Object


    let createRecordData (rd: RecordDocument) =
        let (summary, examples, returns) = handleXmlDoc rd.XmlDocument

        [ "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "name", Mustache.Value.Scalar rd.DisplayName
          "fields",
          Mustache.Value.Array(
              rd.Fields
              |> List.map
                  (fun f ->
                      let (fSummary, fExamples, fReturns) = handleXmlDoc f.XmlDocument

                      [ "summary", Mustache.Value.Scalar(fSummary |> Option.defaultValue "")
                        "returns", Mustache.Value.Scalar(fReturns |> Option.defaultValue "")
                        "examples", Mustache.Value.Array(fExamples |> List.map Mustache.Value.Scalar)
                        "name", Mustache.Value.Scalar f.Name
                        "type", Mustache.Value.Scalar f.Type ]
                      |> Map.ofList
                      |> Mustache.Object)
          ) ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createUnionData (ud: UnionDocument) =
        let (summary, examples, returns) = handleXmlDoc ud.XmlDocument

        [ "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "name", Mustache.Value.Scalar ud.DisplayName
          "members",
          Mustache.Value.Array(
              ud.Members
              |> List.map
                  (fun f ->
                      let (fSummary, fExamples, fReturns) = handleXmlDoc f.XmlDocument

                      [ "summary", Mustache.Value.Scalar(fSummary |> Option.defaultValue "")
                        "returns", Mustache.Value.Scalar(fReturns |> Option.defaultValue "")
                        "examples", Mustache.Value.Array(fExamples |> List.map Mustache.Value.Scalar)
                        "name", Mustache.Value.Scalar f.Name
                        "type", Mustache.Value.Scalar f.Type ]
                      |> Map.ofList
                      |> Mustache.Object)
          ) ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createMemberData (m: Member) =
        match m with
        | Member.Class cd -> createClassData cd
        | Member.Function fd -> createFunctionData fd
        | Member.Module md -> failwith ""
        | Member.Namespace nd -> failwith ""
        | Member.Record rd -> createRecordData rd
        | Member.Union ud -> createUnionData ud





    let createData (members: Member list) =
        members
        |> List.fold
            (fun (cAcc, fAcc, mAcc, nAcc, rAcc, uAcc) m ->
                match m with
                | Member.Class cd -> cAcc @ [ createClassData cd ], fAcc, mAcc, nAcc, rAcc, uAcc
                | Member.Function fd -> cAcc, fAcc @ [ createFunctionData fd ], mAcc, nAcc, rAcc, uAcc
                | Member.Module md -> cAcc, fAcc, mAcc, nAcc, rAcc, uAcc
                | Member.Namespace nd -> cAcc, fAcc, mAcc, nAcc, rAcc, uAcc
                | Member.Record rd -> cAcc, fAcc, mAcc, nAcc, rAcc @ [ createRecordData rd ], uAcc
                | Member.Union ud -> cAcc, fAcc, mAcc, nAcc, rAcc, uAcc @ [ createUnionData ud ])
            ([], [], [], [], [], [])
        |> fun (cAcc, fAcc, mAcc, nAcc, rAcc, uAcc) ->
            ({ Values =
                   [ "classes", Mustache.Value.Array cAcc
                     "functions", Mustache.Value.Array fAcc
                     "modules", Mustache.Value.Array mAcc
                     "namespaces", Mustache.Value.Array nAcc
                     "records", Mustache.Value.Array rAcc
                     "unions", Mustache.Value.Array uAcc ]
                   |> Map.ofList
               Partials = Map.empty }: Mustache.Data)



[<EntryPoint>]
let main argv =
    // "C:\\Users\\44748\\Projects\\TestRepo\\TestRepo\\bin\\Debug\\net6.0\\TestRepo.xml"
    // "C:\\Users\\44748\\Projects\\TestRepo\\TestRepo\\Library.fs"

    let r =
        XmlDocExtractor.extract "C:\\ProjectData\\Freql\\Freql.Sqlite.xml"
        |> SourceExtractor.extract "C:\\Users\\44748\\Projects\\Freql\\Freql.Sqlite\\Library.fs"
        |> Linter.run passThru
        |> Templating.createData
        |> fun d ->
            File.ReadAllText "C:\\Users\\44748\\Projects\\__prototypes\\forge\\module_document.mustache"
            |> Mustache.parse 
            |> Mustache.replace d true
        //|> fun r ->
    
    File.WriteAllText("C:\\Users\\44748\\Projects\\__prototypes\\forge\\module_document.html", r)

    //|> Documents.create []
    //|> Documents.render [ Documents.html "C:\\ProjectData\\Documentation\\Test\\test.html"
    //                      Documents.pdf
    //                          "C:\\ProjectData\\Documentation\\Test\\test.pdf"
    //                          "C:\\Users\\44748\\Projects\\PDFBuilder\\styles.json" ]

    0 // return an integer exit code
