// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FXD

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    // "C:\\Users\\44748\\Projects\\PsionicServices\\PsionicServices.Utils\\Encryption.fs"
    let s = SourceExtractor.parseAndCheckScript "C:\\Users\\44748\\Projects\\TestLibrary\\TestLibrary\\Library.fs"
    let assembly = s.AssemblySignature
    
    let r = assembly.Entities |> List.ofSeq |> List.map SourceExtractor.create
    
    printfn $"{r}"


    let members = XmlDocExtractor.extract "C:\\Users\\44748\\Projects\\TestLibrary\\TestLibrary\\bin\\Debug\\net5.0\\TestLibrary.xml"

    // Parse [Project name].xml docs.
    // Parse file.
    // Combine.  
    
    
    
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code