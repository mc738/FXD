// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FXD
open FXD
open FXD.Documentation

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

let passThru (members: Member list) (errors: Linter.LintingError list) =
    Console.ForegroundColor <- ConsoleColor.Magenta
    errors |> List.map (fun e -> printfn $"{e}") |> ignore
    Console.ResetColor()
    Ok members

[<EntryPoint>]
let main argv =
    
    let members =
        XmlDocExtractor.extract "C:\\Users\\44748\\Projects\\TestLibrary\\TestLibrary\\bin\\Debug\\net5.0\\TestLibrary.xml"
        |> SourceExtractor.extract "C:\\Users\\44748\\Projects\\TestLibrary\\TestLibrary\\Library.fs"
        |> Linter.run passThru
        
    printfn $"{members}"
    
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code