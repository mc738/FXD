// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Reflection.Metadata
open FXD
open FXD
open FXD.Documentation

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
    
    XmlDocExtractor.extract "C:\\ProjectData\\Freql\\Freql.Sqlite.xml"
    |> SourceExtractor.extract "C:\\Users\\44748\\Projects\\Freql\\Freql.Sqlite\\Library.fs"
    |> Linter.run passThru
    |> Documents.create []
    |> Documents.render [ Documents.html "C:\\ProjectData\\Documentation\\Test\\test.html"
                          Documents.pdf
                              "C:\\ProjectData\\Documentation\\Test\\test.pdf"
                              "C:\\Users\\44748\\Projects\\PDFBuilder\\styles.json" ]
    0 // return an integer exit code
