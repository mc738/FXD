// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO
open System.Text.Json
open FXD

(*
module NewStd =

    let createMethodData (m: MethodDocument) =
        let (summary, examples, returns) = Templating.handleXmlDoc m.XmlDocument

        [ "id", Mustache.Value.Scalar m.Id
          "name", Mustache.Value.Scalar m.Name
          "signature", Mustache.Value.Scalar m.Signature
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "parameters",
          m.Parameters
          |> List.map (fun p ->
              [ "name", Mustache.Value.Scalar p.Name
                "type", Mustache.Value.Scalar p.Type
                "document", Mustache.Value.Scalar(p.Document |> Option.defaultValue "") ]
              |> Map.ofList
              |> Mustache.Value.Object)
          |> Mustache.Value.Array
          "examples",
          examples
          |> List.map Mustache.Value.Scalar
          |> Mustache.Value.Array
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "") ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createPropertyData (p: PropertyDocument) =
        let (summary, examples, returns) = Templating.handleXmlDoc p.XmlDocument

        [ "id", Mustache.Value.Scalar p.Id
          "name", Mustache.Value.Scalar p.Name
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "") ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createMethods (ms: MethodDocument list) =
        match ms.IsEmpty with
        | true -> None
        | false ->
            [ "items", ms |> List.map createMethodData |> Mustache.Array ]
            |> Map.ofList
            |> Mustache.Value.Object
            |> Some

    let createProperties (ms: PropertyDocument list) =
        match ms.IsEmpty with
        | true -> None
        | false ->
            [ "items",
              ms
              |> List.map createPropertyData
              |> Mustache.Array ]
            |> Map.ofList
            |> Mustache.Value.Object
            |> Some

    let createClassData (c: ClassDocument) =
        let (summary, examples, returns) = Templating.handleXmlDoc c.XmlDocument

        [ "id", Mustache.Value.Scalar c.Id
          "name", Mustache.Value.Scalar c.DisplayName
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          match createMethods c.Methods with
          | Some m -> "methods", m
          | None -> ()
          match createProperties c.Properties with
          | Some p -> "properties", p
          | None -> () ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createRecordData (r: RecordDocument) =
        let (summary, examples, returns) = Templating.handleXmlDoc r.XmlDocument

        [ "id", Mustache.Value.Scalar r.Id
          "name", Mustache.Value.Scalar r.DisplayName
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "fields",
          r.Fields
          |> List.map (fun f ->
              [ "name", Mustache.Value.Scalar f.Name
                "type", Mustache.Value.Scalar f.Type ]
              |> Map.ofList
              |> Mustache.Value.Object)
          |> Mustache.Value.Array
          match createMethods r.Methods with
          | Some m -> "methods", m
          | None -> ()
          match createProperties r.Properties with
          | Some p -> "properties", p
          | None -> () ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createUnionData (u: UnionDocument) =
        let (summary, examples, returns) = Templating.handleXmlDoc u.XmlDocument

        [ "id", Mustache.Value.Scalar u.Id
          "name", Mustache.Value.Scalar u.DisplayName
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "union_cases",
          u.Members
          |> List.map (fun uc ->
              [ "name", Mustache.Value.Scalar uc.Name
                "type", Mustache.Value.Scalar uc.Type ]
              |> Map.ofList
              |> Mustache.Value.Object)
          |> Mustache.Value.Array
          match createMethods u.Methods with
          | Some m -> "methods", m
          | None -> ()
          match createProperties u.Properties with
          | Some p -> "properties", p
          | None -> () ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createFunctionParameterData (p: FunctionParameter) =
        [ "name", Mustache.Value.Scalar p.Name
          "type", Mustache.Value.Scalar p.Type
          "document", Mustache.Value.Scalar(p.Document |> Option.defaultValue "") ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createFunctionData (f: FunctionDocument) =
        let (summary, examples, returns) = Templating.handleXmlDoc f.XmlDocument

        [ "id", Mustache.Value.Scalar f.Id
          "name", Mustache.Value.Scalar f.DisplayName
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "parameters",
          f.Parameters
          |> List.map createFunctionParameterData
          |> Mustache.Value.Array
          "examples",
          examples
          |> List.map Mustache.Value.Scalar
          |> Mustache.Value.Array
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "") ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createModuleData (index: string) (title: string) (m: ModuleDocument) =
        let classes = m.GetClasses() |> List.map createClassData //|> Mustache.Value.Array

        let records = m.GetRecords() |> List.map createRecordData

        let unions = m.GetUnions() |> List.map createUnionData

        let functions = m.GetFunctions() |> List.map createFunctionData

        [ "index", Mustache.Value.Scalar index
          "doc_name", Mustache.Value.Scalar $"Peeps API - {m.DisplayName}"
          "name", Mustache.Value.Scalar m.DisplayName
          "title", Mustache.Value.Scalar title
          "namespace", Mustache.Value.Scalar m.Namespace
          // TODO add this outside
          "fa_url", Mustache.Value.Scalar "https://kit.fontawesome.com/f5ae0cbcfc.js"
          if classes |> List.isEmpty |> not then
              "class_collection",
              [ "classes", Mustache.Value.Array classes ]
              |> Map.ofList
              |> Mustache.Value.Object

          if records |> List.isEmpty |> not then
              "record_collection",
              [ "records", Mustache.Value.Array records ]
              |> Map.ofList
              |> Mustache.Value.Object

          if unions |> List.isEmpty |> not then
              "union_collection",
              [ "unions", Mustache.Value.Array unions ]
              |> Map.ofList
              |> Mustache.Value.Object

          if functions.IsEmpty |> not then
              "function_collection",
              [ "functions", Mustache.Value.Array functions ]
              |> Map.ofList
              |> Mustache.Value.Object

          ]
        |> Map.ofList

    let generateIndex
        (indexEntries: Map<string, string>)
        (sectionId: string)
        (sectionName: string)
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

        |> String.concat ""
        |> fun indexes -> $"""<div class="index-section"><h2>API reference</h2><ul>{indexes}</ul></div>"""

    let generateModulePage
        (template: Mustache.Token list)
        (savePath: string)
        (sectionName: string)
        (indexEntries: Map<string, string>)
        (regexIgnore: string)
        (m: ModuleDocument)
        =
        let index =
            generateIndex indexEntries m.Id sectionName m.DisplayName regexIgnore m.Members

        ({ Values = createModuleData index sectionName m
           Partials = Map.empty }: Mustache.Data)
        |> fun d -> Mustache.replace d true template
        |> fun r -> File.WriteAllText(Path.Combine(savePath, $"{m.Id}.html"), r)

    let createIndex (regexIgnore: string) (modules: ModuleDocument list) =
        modules
        |> List.map (fun m ->
            match Regex.IsMatch(m.FullName, regexIgnore) with
            | true -> None
            | false -> Some(m.Id, $"""<li><a href="./{m.Id}.html">{m.DisplayName}</a></li>"""))
        |> List.choose id
        |> Map.ofList

    let createModules
        (template: Mustache.Token list)
        (savePath: string)
        (sectionName: string)
        (regexIgnore: string)
        (members: Member list)
        =
        // TODO Handle top level non module.
        let (modules, topLevel) =
            members
            |> List.fold
                (fun (acc, topLevel) m ->
                    match m with
                    | Member.Module mm -> acc @ [ mm ], topLevel
                    | _ -> acc, topLevel @ [])
                ([], [])
        //|> fun (acc, topLevel) ->
        //    ModuleDocument.Create()

        // TODO create index.
        let indexes = createIndex regexIgnore modules

        modules
        |> List.iter (generateModulePage template savePath sectionName indexes regexIgnore)

module TestReport =
    let createTestResults (testRun: TestRunReport.TestRun) =

        testRun.Results
        |> List.map (fun tr ->
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
*)


// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

(*
let passThru (members: Member list) (errors: Linter.LintingError list) =
    Console.ForegroundColor <- ConsoleColor.Magenta

    errors
    |> List.map (fun e -> printfn $"{e}")
    |> ignore

    Console.ResetColor()
    members
*)

[<EntryPoint>]
let main argv =

    let templateCache =
        FXD
            .Pipelines
            .Impl
            .TemplateCache
            .Empty
            .LoadAndAdd("article", "C:\\Users\\44748\\Projects\\FXD\\Templates\\article.mustache")
            .LoadAndAdd("code_doc", "C:\\Users\\44748\\Projects\\FXD\\Templates\\module_document_std.mustache")

    let cfg =
        File.ReadAllText "C:\\ProjectData\\fdx\\test_config.json"
        |> JsonSerializer.Deserialize<Pipelines.Configuration.PipelineConfiguration>


    let ctx =
        ({ Name = "Peeps"
           Templates = templateCache
           RootPath = "C:\\Users\\44748\\Projects\\Peeps"
           OutputRoot = "C:\\ProjectData\\Peeps\\docs"
           Configuration = cfg }: Pipelines.Impl.Context)


    ctx.Run()

    // "C:\\Users\\44748\\Projects\\TestRepo\\TestRepo\\bin\\Debug\\net6.0\\TestRepo.xml"
    // "C:\\Users\\44748\\Projects\\TestRepo\\TestRepo\\Library.fs"

    (*
    let paths =
        [ "C:\\Users\\44748\\Projects\\Freql\\Freql.Sqlite\\Library.fs"
          "C:\\Users\\44748\\Projects\\Freql\\Freql.MySql\\Library.fs" ]

    let peepsPaths =
        [ "C:\\Users\\44748\\Projects\\Peeps\\Peeps\\Core.fs"
          "C:\\Users\\44748\\Projects\\Peeps\\Peeps\\Logger.fs"
          "C:\\Users\\44748\\Projects\\Peeps\\Peeps\\Store.fs"
          "C:\\Users\\44748\\Projects\\Peeps\\Peeps\\Extensions.fs"
          "C:\\Users\\44748\\Projects\\Peeps\\Peeps\\Actions.fs" ]

    let ignoreRegex = "QueryHelpers"

    let r = SourceExtractor.extractMultiple peepsPaths "QueryHelpers|Internal"

    let template =
        File.ReadAllText "C:\\Users\\44748\\Projects\\FXD\\Templates\\module_document_std.mustache"
        |> Mustache.parse

    NewStd.createModules
        template
        "C:\\ProjectData\\Peeps\\docs\\api"
        "Peeps"
        "Internal"
        (r |> Map.toList |> List.map (fun (_, v) -> v)).[0]

    TestReport.generate
        "C:\\ProjectData\\Misc\\example_test_result.xml"
        "C:\Users\\44748\\Projects\\FXD\\Templates\\test_report.mustache"
        "C:\\ProjectData\\test_docs\\test_report.html"


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
    *)
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
