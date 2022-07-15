namespace FXD.Pipelines

module ConfigurationV1 =

    open System.Text.Json.Serialization

    [<CLIMutable>]
    type PipelineConfiguration =
        { [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("args")>]
          Args: Arg seq
          [<JsonPropertyName("metadata")>]
          Metadata: MetadataItem seq
          [<JsonPropertyName("directories")>]
          Directories: string seq
          [<JsonPropertyName("pages")>]
          Pages: PageConfiguration seq
          [<JsonPropertyName("codeDocuments")>]
          CodeDocuments: CodeDocumentConfiguration
          [<JsonPropertyName("resources")>]
          Resources: ResourceConfiguration seq
          [<JsonPropertyName("indexes")>]
          Indexes: IndexItem seq }

    and [<CLIMutable>] Arg =
        { [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("default")>]
          Default: string
          [<JsonPropertyName("required")>]
          Required: bool }

    and [<CLIMutable>] PageConfiguration =
        { [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("template")>]
          Template: string
          [<JsonPropertyName("source")>]
          Source: string
          [<JsonPropertyName("title")>]
          Title: string
          [<JsonPropertyName("titleSlug")>]
          TitleSlug: string
          [<JsonPropertyName("output")>]
          Output: string
          [<JsonPropertyName("metadata")>]
          Metadata: MetadataItem seq }

    and [<CLIMutable>] CodeDocumentConfiguration =
        { [<JsonPropertyName("namespaceTemplate")>]
          NamespaceTemplate: string
          [<JsonPropertyName("classTemplate")>]
          ClassTemplate: string
          [<JsonPropertyName("recordTemplate")>]
          RecordTemplate: string
          [<JsonPropertyName("unionTemplate")>]
          UnionTemplate: string
          [<JsonPropertyName("functionTemplate")>]
          FunctionTemplate: string
          [<JsonPropertyName("moduleTemplate")>]
          ModuleTemplate: string
          [<JsonPropertyName("directoryName")>]
          DirectoryName: string
          [<JsonPropertyName("sources")>]
          Sources: CodeDocumentSource seq }

    and [<CLIMutable>] CodeDocumentSource =
        { [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("source")>]
          Source: string
          [<JsonPropertyName("metadata")>]
          Metadata: MetadataItem seq }

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

    and [<CLIMutable>] IndexItem =
        { [<JsonPropertyName("sectionName")>]
          SectionName: string
          [<JsonPropertyName("type")>]
          Type: string
          [<JsonPropertyName("pages")>]
          Pages: string seq }
        
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
          [<JsonPropertyName("items")>]
          Items: string seq }
    
