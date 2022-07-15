namespace FXD.CodeDocuments.FSharp

[<AutoOpen>]
module Common =

    open FSharp.Compiler.Symbols

    let displayContext =
        FSharpDisplayContext.Empty
        |> fun ctx -> ctx.WithSuffixGenericParameters()
        |> fun ctx -> ctx.WithShortTypeNames true
        
        
    
