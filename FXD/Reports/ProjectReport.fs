namespace FXD.Reports

open FsToolbox.Core

[<RequireQualifiedAccess>]
module ProjectReport =

    open System.IO
    open System.Text.RegularExpressions
    open Fluff.Core

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
        |> Result.bind (fun doc ->
            Xml.getElements "ItemGroup" doc.Root
            |> List.map (fun ig ->
                let projectDependencies =
                    Xml.getElements "ProjectReference" ig
                    |> List.map (fun pr ->
                        match Xml.tryGetAttribute "Include" pr with
                        | Some a ->
                            ({ Name = getNameFromProj a.Value
                               Path = a.Value }: ProjectDependency)
                            |> Ok
                        | None -> Error "Malformed element (missing Include attribute).")

                let packageDependencies =
                    Xml.getElements "PackageReference" ig
                    |> List.map (fun pr ->
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
                        |> List.map (fun r ->
                            match r with
                            | Ok sr -> Some sr
                            | Error _ -> None)
                        |> List.choose id
                        |> fun r -> accProjs @ r

                    let newAccPacks =
                        packs
                        |> List.map (fun r ->
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
                |> List.map (fun fi ->
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
            [ "package_name", Mustache.Value.Scalar packageDependency.Name
              "package_version", Mustache.Value.Scalar packageDependency.Version ]
            |> Map.ofList
        )

    let createPackageReferenceObjs (pds: PackageDependency list) =
        pds
        |> List.map createPackageReferenceObj
        |> fun r -> Mustache.Value.Array r

    let createProjectReferenceObj (packageDependency: ProjectDependency) =
        Mustache.Value.Object(
            [ "project_name", Mustache.Value.Scalar packageDependency.Name
              "project_path", Mustache.Value.Scalar packageDependency.Path ]
            |> Map.ofList
        )

    let createProjectReferenceObjs (pds: ProjectDependency list) =
        pds
        |> List.map createProjectReferenceObj
        |> fun r -> Mustache.Value.Array r

    let extract (path: string) =
        getSolutionDependencies path
        |> List.choose (fun pd ->
            match pd with
            | Ok pd -> Some pd
            | _ -> None)
        |> List.map (fun pd ->
            Mustache.Value.Object(
                [ "project_name", Mustache.Value.Scalar pd.Name
                  if pd.PackageDependencies.IsEmpty |> not then
                      "package_dependencies",
                      [ "package_references", createPackageReferenceObjs pd.PackageDependencies ]
                      |> Map.ofList
                      |> Mustache.Value.Object

                  if pd.ProjectDependencies.IsEmpty |> not then
                      "project_dependencies",
                      [ "project_references", createProjectReferenceObjs pd.ProjectDependencies ]
                      |> Map.ofList
                      |> Mustache.Value.Object ]
                |> Map.ofList
            ))
    
    let generate (template: string) (path: string) =
        getSolutionDependencies path
        |> List.choose (fun pd ->
            match pd with
            | Ok pd -> Some pd
            | _ -> None)
        |> List.map (fun pd ->
            Mustache.Value.Object(
                [ "project_name", Mustache.Value.Scalar pd.Name
                  if pd.PackageDependencies.IsEmpty |> not then
                      "package_dependencies",
                      [ "package_references", createPackageReferenceObjs pd.PackageDependencies ]
                      |> Map.ofList
                      |> Mustache.Value.Object

                  if pd.ProjectDependencies.IsEmpty |> not then
                      "project_dependencies",
                      [ "project_references", createProjectReferenceObjs pd.ProjectDependencies ]
                      |> Map.ofList
                      |> Mustache.Value.Object ]
                |> Map.ofList
            ))
        |> fun r ->
            ({ Values =
                [ "projects", Mustache.Value.Array r ]
                |> Map.ofList
               Partials = Map.empty }: Mustache.Data)
        |> fun v -> Mustache.parse template |> Mustache.replace v true
