// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Reflection.Metadata
open System.Text.Json
open FXD
open FXD
open FXD.Documentation
open FXD.Reports
open Fluff.Core
open FXD.XmlDocExtractor


module TestReport =


    let createTestResults (testRun: TestRunReport.TestRun) =

        testRun.Results
        |> List.map
            (fun tr ->
                let test =
                    testRun.TestDefinitions
                    |> List.tryFind (fun t -> t.Id = tr.TestId)
                    |> Option.defaultWith (fun _ -> failwith $"Could not find test {tr.TestId}")

                let outcome = tr.Outcome = "Passed"

                [ "testResultClass",
                  Mustache.Value.Scalar(
                      match outcome with
                      | true -> "fas fa-check-circle success"
                      | false -> "fas fa-times-circle danger"
                  )
                  "duration", Mustache.Value.Scalar <| tr.Duration.ToString()
                  "testName", Mustache.Value.Scalar test.Name
                  "testId", Mustache.Value.Scalar <| test.Id.ToString()
                  "executionId", Mustache.Value.Scalar <| tr.ExecutionId.ToString()
                  "codebase", Mustache.Value.Scalar test.TestMethod.CodeBase
                  "storage", Mustache.Value.Scalar test.Storage
                  "adapter", Mustache.Value.Scalar test.TestMethod.AdapterTypeName
                  "className", Mustache.Value.Scalar test.TestMethod.ClassName ]
                |> Map.ofList
                |> Mustache.Value.Object)
        |> Mustache.Value.Array
        |> fun r ->
            [ "resultStyle", Mustache.Value.Scalar "fas fa-check-circle success"
              "testRunName", Mustache.Value.Scalar testRun.Name
              "runDeploymentRoot", Mustache.Value.Scalar testRun.Settings.Deployment.RunDeploymentRoot
              "runId", Mustache.Value.Scalar <| testRun.Id.ToString()
              "testTime",
              Mustache.Value.Scalar
              <| testRun.Times.Creation.ToString()
              "created",
              Mustache.Value.Scalar
              <| testRun.Times.Creation.ToString()
              "queued",
              Mustache.Value.Scalar
              <| testRun.Times.Queuing.ToString()
              "start",
              Mustache.Value.Scalar
              <| testRun.Times.Start.ToString()
              "finish",
              Mustache.Value.Scalar
              <| testRun.Times.Finish.ToString()
              "tests", r ]
            |> Map.ofList
            |> Mustache.Object
        |> fun r ->
            [ "testRuns", Mustache.Value.Array [ r ] ]
            |> Map.ofList
    //|> Mustache.Value.Object

    let generate (path: string) (templatePath: string) (output: string) =
        match Reports.TestRunReport.generate path with
        | Ok testRun ->
            createTestResults testRun
            |> fun d ->
                Mustache.parse (File.ReadAllText templatePath)
                |> fun t ->
                    ({ Values = d; Partials = Map.empty }: Mustache.Data)
                    |> fun d -> Mustache.replace d true t
                    |> fun r -> File.WriteAllText(output, r)
        | Error f -> failwith $"Could not load test results. Error: {f.Message}"


        ()



// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

let passThru (members: Member list) (errors: Linter.LintingError list) =
    Console.ForegroundColor <- ConsoleColor.Magenta

    errors
    |> List.map (fun e -> printfn $"{e}")
    |> ignore

    Console.ResetColor()
    members

[<EntryPoint>]
let main argv =
    // "C:\\Users\\44748\\Projects\\TestRepo\\TestRepo\\bin\\Debug\\net6.0\\TestRepo.xml"
    // "C:\\Users\\44748\\Projects\\TestRepo\\TestRepo\\Library.fs"


    TestReport.generate "C:\\ProjectData\\Misc\\example_test_result.xml" "C:\Users\\44748\\Projects\\FXD\\Templates\\test_report.mustache" "C:\\ProjectData\\test_docs\\test_report.html"
    
    
    let testRun =
        Reports.TestRunReport.generate "C:\\ProjectData\\Misc\\example_test_result.xml"
    //|> ignore



    let ctx =
        [ "version", "v.0.1.0"
          "message", "Hello, World!" ]
        |> Map.ofList
        |> Pipeline.load "C:\\Users\\44748\\Projects\\Freql" "C:\\ProjectData\\Freql\\website-new-style"

    //let config = File.ReadAllText "C:\\ProjectData\\Freql\\.fxd\\build.json" |> JsonSerializer.Deserialize<Documents.Configuration>


    Pipeline.run ctx

    (*
    let r =
        //XmlDocExtractor.extract "C:\\ProjectData\\Freql\\Freql.Sqlite.xml"
        SourceExtractor.extract "C:\\Users\\44748\\Projects\\Freql\\Freql.Sqlite\\Library.fs"
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
    *)
    0 // return an integer exit code
