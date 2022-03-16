﻿namespace FXD

open System
open System.IO
open System.Text.RegularExpressions
open System.Xml.Linq
open Fluff.Core
open ToolBox.AppEnvironment.ArgParser
open ToolBox.Core

module Reports =

    [<RequireQualifiedAccess>]
    module ProjectReport =

        type PackageVersion = { Name: string }

        type DependencyType =
            | Project
            | Package of PackageVersion

        type Dependency = { Path: string; Type: DependencyType }

        type PackageDependency = { Name: string; Version: string }

        type ProjectDependency = { Name: string; Path: string }

        type ProjectDetails =
            { Name: string
              ProjectFilePath: string
              PackageDependencies: PackageDependency list
              ProjectDependencies: ProjectDependency list }

        let getNameFromProj (path: string) =
            Path.GetFileName path
            |> fun n -> Regex.Replace(n, "(?<fsharp>.fsproj$)|(?<csharp>.csproj$)", "")

        let getDependencies (path: string) =
            // Find the relevant
            FileIO.readText path
            |> Result.bind Xml.parse
            |> Result.bind
                (fun doc ->
                    Xml.getElements "ItemGroup" doc.Root
                    |> List.map
                        (fun ig ->
                            let projectDependencies =
                                Xml.getElements "ProjectReference" ig
                                |> List.map
                                    (fun pr ->
                                        match Xml.tryGetAttribute "Include" pr with
                                        | Some a ->
                                            ({ Name = getNameFromProj a.Value
                                               Path = a.Value }: ProjectDependency)
                                            |> Ok
                                        | None -> Error "Malformed element (missing Include attribute).")

                            let packageDependencies =
                                Xml.getElements "PackageReference" ig
                                |> List.map
                                    (fun pr ->
                                        match Xml.tryGetAttribute "Include" pr, Xml.tryGetAttribute "Version" pr with
                                        | Some ia, Some va ->
                                            ({ Name = ia.Value; Version = va.Value }: PackageDependency)
                                            |> Ok

                                        | _ -> Error "Malformed element (missing Include or Version attribute)")

                            projectDependencies, packageDependencies)
                    |> List.fold
                        (fun (accProjs, accPacks) (projs, packs) ->

                            let newAccProjs =
                                projs
                                |> List.map
                                    (fun r ->
                                        match r with
                                        | Ok sr -> Some sr
                                        | Error _ -> None)
                                |> List.choose id
                                |> fun r -> accProjs @ r

                            let newAccPacks =
                                packs
                                |> List.map
                                    (fun r ->
                                        match r with
                                        | Ok sr -> Some sr
                                        | Error _ -> None)
                                |> List.choose id
                                |> fun r -> accPacks @ r

                            newAccProjs, newAccPacks)
                        ([], [])
                    |> fun (projs, packs) ->
                        ({ Name = getNameFromProj path
                           ProjectFilePath = path
                           ProjectDependencies = projs
                           PackageDependencies = packs }: ProjectDetails)
                        |> Ok)

        let getSolutionDependencies (path: string) =
            let rec searchLoop (dir: string) =

                let di = DirectoryInfo(dir)

                let pds =
                    di.EnumerateFiles()
                    |> List.ofSeq
                    |> List.filter (fun fi -> Regex.IsMatch(fi.Name, ".fsproj$|.csproj$"))
                    |> List.map
                        (fun fi ->
                            printfn $"{getNameFromProj fi.Name}"
                            getDependencies fi.FullName)

                let childrenPds =
                    di.EnumerateDirectories()
                    |> List.ofSeq
                    |> List.map (fun di -> searchLoop (di.FullName))
                    |> List.concat

                pds @ childrenPds

            searchLoop (path)

        let createPackageReferenceObj (packageDependency: PackageDependency) =
            Mustache.Value.Object(
                [ "packageName", Mustache.Value.Scalar packageDependency.Name
                  "packageVersion", Mustache.Value.Scalar packageDependency.Version ]
                |> Map.ofList
            )

        let createPackageReferenceObjs (pds: PackageDependency list) =
            pds
            |> List.map createPackageReferenceObj
            |> fun r -> Mustache.Value.Array r

        let createProjectReferenceObj (packageDependency: ProjectDependency) =
            Mustache.Value.Object(
                [ "projectName", Mustache.Value.Scalar packageDependency.Name
                  "projectPath", Mustache.Value.Scalar packageDependency.Path ]
                |> Map.ofList
            )

        let createProjectReferenceObjs (pds: ProjectDependency list) =
            pds
            |> List.map createProjectReferenceObj
            |> fun r -> Mustache.Value.Array r

        let generate (template: string) (path: string) =
            getSolutionDependencies path
            |> List.choose
                (fun pd ->
                    match pd with
                    | Ok pd -> Some pd
                    | _ -> None)
            |> List.map
                (fun pd ->
                    Mustache.Value.Object(
                        [ "projectName", Mustache.Value.Scalar pd.Name
                          "packageReferences", createPackageReferenceObjs pd.PackageDependencies
                          "projectReferences", createProjectReferenceObjs pd.ProjectDependencies ]
                        |> Map.ofList
                    ))
            |> fun r ->
                ({ Values =
                       [ "projects", Mustache.Value.Array r ]
                       |> Map.ofList
                   Partials = Map.empty }: Mustache.Data)
            |> fun v -> Mustache.parse template |> Mustache.replace v true


    [<RequireQualifiedAccess>]
    module TestRunReport =

        type TestRun =
            { Id: Guid
              Name: string
              RunUser: string
              Times: Times
              Settings: TestSettings
              Results: UnitTestResult list
              TestDefinitions: UnitTestDefinition list
              TestEntries: TestEntry list
              TestLists: TestList list
              ResultSummary: ResultSummary }

        and Times =
            { Creation: DateTime
              Queuing: DateTime
              Start: DateTime
              Finish: DateTime }

        and TestSettings =
            { Id: Guid
              Name: string
              Deployment: Deployment }

        and Deployment = { RunDeploymentRoot: string }

        and UnitTestResult =
            { ExecutionId: Guid
              TestId: Guid
              TestName: string
              ComputerName: string
              Duration: TimeSpan
              StartTime: DateTime
              EndTime: DateTime
              TestType: Guid
              Outcome: string
              TestListId: Guid
              RelativeResultsDirectory: Guid }

        and UnitTestDefinition =
            { Name: string
              Storage: string
              Id: Guid
              Execution: Execution
              TestMethod: TestMethod }

        and Execution = { Id: Guid }

        and TestMethod =
            { CodeBase: string
              AdapterTypeName: string
              ClassName: string
              Name: string }

        and TestEntry =
            { TestId: Guid
              ExecutionId: Guid
              TestListId: Guid }

        and TestList = { Name: string; Id: Guid }

        and ResultSummary = { Outcome: string; Counters: Counters }

        and Counters =
            { Total: int
              Executed: int
              Passed: int
              Failed: int
              Error: int
              Timeout: int
              Aborted: int
              Inconclusive: int
              PassedButRunAborted: int
              NotRunnable: int
              NotExecuted: int
              Disconnected: int
              Warning: int
              Completed: int
              InProgress: int
              Pending: int }

        module Internal =
            
            let xName name = Xml.xName "http://microsoft.com/schemas/VisualStudio/TeamTest/2010" name
            
            let parseResults (path: string) =
                FileIO.readText path |> Result.bind Xml.parse

            let createTimes (root: XElement) =
                Xml.tryGetElement (xName "Times")  root
                |> fun el ->
                    match el with
                    | Some el ->
                        ({ Creation = Xml.getDateTimeAttribute "creation" el
                           Queuing = Xml.getDateTimeAttribute "queuing" el
                           Start = Xml.getDateTimeAttribute "start" el
                           Finish = Xml.getDateTimeAttribute "finish" el }: Times)
                    | None -> failwith "Missing `Times` element."

            let createSettings (root: XElement) =
                Xml.tryGetElement (xName "TestSettings") root
                |> fun el ->
                    match el with
                    | Some el ->
                        ({ Name = Xml.getAttributeValue "name" el
                           Id = Xml.getGuidAttributeValue "id" el
                           Deployment =
                               { RunDeploymentRoot =
                                     Xml.tryGetElement (xName "Deployment") el
                                     |> (fun subEl ->
                                         match subEl with
                                         | Some subEl -> Xml.getAttributeValue "runDeploymentRoot" subEl
                                         | None -> failwith "Missing `Deployment` element") } }: TestSettings)
                    | None -> failwith "Missing `TestSettings` element"

            let createResults (root: XElement) =
                match Xml.tryGetElement (xName "Results") root with
                | Some el ->
                    Xml.getElements (xName "UnitTestResult") el
                    |> List.map
                        (fun el ->
                            ({ ExecutionId = Xml.getGuidAttributeValue "executionId" el
                               TestId = Xml.getGuidAttributeValue "testId" el
                               TestName = Xml.getAttributeValue "testName" el
                               ComputerName = Xml.getAttributeValue "computerName" el
                               Duration = Xml.getTimespanAttribute "duration" el
                               StartTime = Xml.getDateTimeAttribute "startTime" el
                               EndTime = Xml.getDateTimeAttribute "endTime" el
                               TestType = Xml.getGuidAttributeValue "testType" el
                               Outcome = Xml.getAttributeValue "outcome" el
                               TestListId = Xml.getGuidAttributeValue "testListId" el
                               RelativeResultsDirectory = Xml.getGuidAttributeValue "relativeResultsDirectory" el }: UnitTestResult))

                | None -> failwith "Missing `Results` element"

            let createTestDefinitions (root: XElement) =
                match Xml.tryGetElement (xName "TestDefinitions") root with
                | Some el ->
                    Xml.getElements (xName "UnitTest") el
                    |> List.map
                        (fun el ->
                            ({ Name = Xml.getAttributeValue "name" el
                               Storage = Xml.getAttributeValue "storage" el
                               Id = Xml.getGuidAttributeValue "id" el
                               Execution =
                                   match Xml.tryGetElement (xName "Execution") el with
                                   | Some el -> ({ Id = Xml.getGuidAttributeValue "id" el }: Execution)
                                   | None -> failwith "Missing `Execution` element"
                               TestMethod =
                                   match Xml.tryGetElement (xName "TestMethod") el with
                                   | Some el ->
                                       ({ CodeBase = Xml.getAttributeValue "codeBase" el
                                          AdapterTypeName = Xml.getAttributeValue "adapterTypeName" el
                                          ClassName = Xml.getAttributeValue "className" el
                                          Name = Xml.getAttributeValue "name" el }: TestMethod)
                                   | None -> failwith "Missing `TestMethod` element" }: UnitTestDefinition))
                | None -> failwith "Missing `TestDefinitions` element"

            let createTestEntries (root: XElement) =
                Xml.tryGetElement (xName "TestEntries") root
                |> fun el ->
                    match el with
                    | Some el ->
                        Xml.getElements (xName "TestEntry") el
                        |> List.map
                            (fun el ->
                                ({ TestId = Xml.getGuidAttributeValue "testId" el
                                   ExecutionId = Xml.getGuidAttributeValue "executionId" el
                                   TestListId = Xml.getGuidAttributeValue "testListId" el }: TestEntry))
                    | None -> failwith "Missing `Deployment` element"

            let createTestLists (root: XElement) =
                match Xml.tryGetElement (xName "TestLists") root with
                | Some el ->
                    Xml.getElements (xName "TestList") el
                    |> List.map
                        (fun el ->
                            ({ Name = Xml.getAttributeValue "name" el
                               Id = Xml.getGuidAttributeValue "id" el }: TestList))
                | None -> failwith "Missing `TestLists` element"

            let createResultSummary (root: XElement) =
                match Xml.tryGetElement (xName "ResultSummary") root with
                | Some el ->
                    ({ Outcome = Xml.getAttributeValue "outcome" el
                       Counters =
                           match Xml.tryGetElement (xName "Counters") el with
                           | Some el ->
                               ({ Total = Xml.getIntAttributeValue "total" el
                                  Executed = Xml.getIntAttributeValue "executed" el
                                  Passed = Xml.getIntAttributeValue "passed" el
                                  Failed = Xml.getIntAttributeValue "failed" el
                                  Error = Xml.getIntAttributeValue "error" el
                                  Timeout = Xml.getIntAttributeValue "timeout" el
                                  Aborted = Xml.getIntAttributeValue "aborted" el
                                  Inconclusive = Xml.getIntAttributeValue "inconclusive" el
                                  PassedButRunAborted = Xml.getIntAttributeValue "passedButRunAborted" el
                                  NotRunnable = Xml.getIntAttributeValue "notRunnable" el
                                  NotExecuted = Xml.getIntAttributeValue "notExecuted" el
                                  Disconnected = Xml.getIntAttributeValue "disconnected" el
                                  Warning = Xml.getIntAttributeValue "warning" el
                                  Completed = Xml.getIntAttributeValue "completed" el
                                  InProgress = Xml.getIntAttributeValue "inProgress" el
                                  Pending = Xml.getIntAttributeValue "pending" el }: Counters)
                           | None -> failwith "Missing `Counters` element" }: ResultSummary)
                | None -> failwith "Missing `ResultSummary` element"

            let createTestRun (doc: XDocument) =
                let root = doc.Root

                ({ Id = Xml.getGuidAttributeValue "id" root
                   Name = Xml.getAttributeValue "name" root
                   RunUser = Xml.getAttributeValue "runUser" root
                   Times = createTimes root
                   Settings = createSettings root
                   Results = createResults root
                   TestDefinitions = createTestDefinitions root
                   TestEntries = createTestEntries root
                   TestLists = createTestLists root
                   ResultSummary = createResultSummary root }: TestRun)

        let generate path =
            Internal.parseResults path
            |> Result.bind (fun d -> Internal.createTestRun d |> Ok)
            
