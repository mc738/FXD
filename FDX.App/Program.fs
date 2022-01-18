// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Reflection.Metadata
open System.Text.Json
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

[<EntryPoint>]
let main argv =
    // "C:\\Users\\44748\\Projects\\TestRepo\\TestRepo\\bin\\Debug\\net6.0\\TestRepo.xml"
    // "C:\\Users\\44748\\Projects\\TestRepo\\TestRepo\\Library.fs"

    let ctx =
        [ "version", "v.0.1.0"
          "message", "Hello, World!" ]
        |> Map.ofList
        |> Pipeline.load "C:\\Users\\44748\\Projects\\Freql" "C:\\ProjectData\\Freql\\website"

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
