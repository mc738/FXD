namespace FXD.Pipelines

open System.IO
open System.Text.Json

module Configuration =

    open System.Text.Json.Serialization
        
    [<CLIMutable>]
    type PipelineConfiguration =
        { [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("outputDirectory")>]
          OutputDirectory: string
          [<JsonPropertyName("args")>]
          Args: Arg seq
          [<JsonPropertyName("metadata")>]
          Metadata: MetadataItem seq
          [<JsonPropertyName("directories")>]
          Directories: string seq
          [<JsonPropertyName("resources")>]
          Resources: ResourceConfiguration seq
          [<JsonPropertyName("sections")>]
          Sections: SectionItem seq }
        
        static member Load(path: string) =
            File.ReadAllText path
            |> JsonSerializer.Deserialize<PipelineConfiguration>

    and [<CLIMutable>] Arg =
        { [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("default")>]
          Default: string
          [<JsonPropertyName("required")>]
          Required: bool }
    
    and [<CLIMutable>] ResourceConfiguration =
        { [<JsonPropertyName("path")>]
          Path: string
          [<JsonPropertyName("outputPath")>]
          outputPath: string }

    and [<CLIMutable>] MetadataItem =
        { [<JsonPropertyName("key")>]
          Key: string
          [<JsonPropertyName("value")>]
          Value: string }
    
    and [<CLIMutable>] SectionItem =
        { [<JsonPropertyName("title")>]
          Title: string
          [<JsonPropertyName("type")>]
          Type: string
          [<JsonPropertyName("template")>]
          Template: string
          [<JsonPropertyName("regexIgnore")>]
          RegexIgnore: string
          [<JsonPropertyName("items")>]
          Items: string seq }