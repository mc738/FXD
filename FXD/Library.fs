namespace FXD

open System
open System.IO
open System.Xml.Linq
open System.Xml.Linq
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Xml


[<AutoOpen>]
module Common =
    let listMap<'In, 'Out> (fn: 'In -> 'Out) (s: seq<'In>) = s |> List.ofSeq |> List.map fn


module XmlDocExtractor =
    
    [<AutoOpen>]
    module Helpers =
        
        let xName (name: string) = XName.op_Implicit name
        
        let getElement (name: string) (el: XElement) =
            let found = el.Element(xName name)
            match found <> null with
            | true -> Some found
            | false -> None
            
        let getAttribute (name: string) (el: XElement) =
            let found = el.Attribute(xName name)
            match found <> null with
            | true -> Some found
            | false -> None
            
    
    let load (path: string) =
        File.ReadAllText path
        |> XDocument.Parse
        |> fun xdoc -> xdoc.Root
    
    
    
    type Parameter = {
        Name: string
        Description: string
    }
    
    
    type XmlDocMember =
        {
            Name: string
            Summary: string option
            Parameters: Parameter list
            Returns: string option
        }
    
    let extract path =
        let root = load path
        
        match root |> getElement "members" with
        | Some el ->
            el.Elements(xName "member")
            |> listMap (fun mel ->
                // Summary
                //let summary = 
                { Name = mel |> getAttribute "name" |> Option.map (fun at -> at.Value) |> Option.defaultValue ""
                  Summary = mel |> getElement "summary" |> Option.map (fun el -> el.Value)
                  Parameters =
                      mel.Elements(xName "param")
                      |> listMap (fun pel ->
                          {
                              Name = pel |> getAttribute "name" |> Option.map (fun at -> at.Value) |> Option.defaultValue ""
                              Description = pel.Value
                          })
                  Returns = mel |> getElement "returns" |> Option.map (fun el -> el.Value)
                }
                // Parameter(s) - multiple
                
                // Returns
                
                // Example(s) - multiple
                
                // Notes
                )
        | None -> failwith "Error"
        
        

/// Extract information from source code.
module SourceExtractor =

    type FunctionDocument =
        { DisplayName: string
          Signature: string
          XDocSignature: string
          Parameters: FunctionParameter list  }

        static member Create(entity: FSharpMemberOrFunctionOrValue) =
            { DisplayName = entity.DisplayName
              Signature = entity.FullType.ToString()
              XDocSignature = entity.XmlDocSig
              Parameters =
                entity.CurriedParameterGroups
                |> listMap (fun p -> p |> listMap FunctionParameter.Create) |> List.concat }
            |> Member.Function

    and FunctionParameter =
        { Name: string
          Type: string }

        static member Create(entity: FSharpParameter) =
            {
                Name = entity.Name |> Option.defaultValue ""
                Type = entity.Type.ToString()
            }

    and UnionDocument =
        { FullName: string
          DisplayName: string
          XDocSignature: string
          Members: UnionMember list }

        static member Create(entity: FSharpEntity) =

            { FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              Members = entity.UnionCases |> listMap (UnionMember.Create) }
            |> Member.Union

    and UnionMember =
        { Name: string
          Type: string }

        static member Create(unionCase: FSharpUnionCase) =
            { Name = unionCase.Name
              Type = unionCase.ReturnType.ToString() }

    and RecordDocument =
        { FullName: string
          DisplayName: string
          XDocSignature: string
          Fields: RecordField list }

        static member Create(entity: FSharpEntity) =
            { FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              Fields =
                  entity.FSharpFields
                  |> List.ofSeq
                  |> List.map RecordField.Create }
            |> Member.Record

    and RecordField =
        { Name: string
          Type: string }

        static member Create(field: FSharpField) =
            { Name = field.Name
              Type = field.FieldType.ToString() }

    and ClassDocument =
        { FullName: string
          DisplayName: string
          XDocSignature: string
          Members: Member list }

        static member Create(entity: FSharpEntity, children: Member list) =
            { FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              Members = children }
            |> Member.Class

    and ModuleDocument =
        { FullName: string
          DisplayName: string
          XDocSignature: string
          Members: Member list }

        static member Create(entity: FSharpEntity, members: Member list) =
            { FullName = entity.FullName
              DisplayName = entity.DisplayName
              XDocSignature = entity.XmlDocSig
              Members = members }
            |> Member.Module

    and NamespaceDocument = { Members: Member list }

    and [<RequireQualifiedAccess>] Member =
        | Function of FunctionDocument
        | Union of UnionDocument
        | Record of RecordDocument
        | Class of ClassDocument
        | Module of ModuleDocument
        | Namespace of NamespaceDocument

    let load (path: string) = File.ReadAllText(path)

    let parseAndCheckScript path =
        let checker =
            FSharpChecker.Create(keepAssemblyContents = true)

        let text = load path

        let projOptions, errors =
            checker.GetProjectOptionsFromScript(path, SourceText.ofString text)
            |> Async.RunSynchronously

        printfn $"Errors: {errors.Length}"

        checker.ParseAndCheckProject(projOptions)
        |> Async.RunSynchronously

    let createFunctions (entity: FSharpEntity) =
        entity.MembersFunctionsAndValues
        |> listMap FunctionDocument.Create

    let rec create (entity: FSharpEntity) =
        match entity with
        | _ when entity.IsFSharpModule ->
            ModuleDocument.Create(
                entity,
                [ entity.NestedEntities |> listMap create
                  entity.MembersFunctionsAndValues
                  |> listMap FunctionDocument.Create ]
                |> List.concat
            )
        | _ when entity.IsFSharpRecord -> RecordDocument.Create entity
        | _ when entity.IsFSharpUnion -> UnionDocument.Create entity
        | _ when entity.IsClass -> ClassDocument.Create(entity, entity.NestedEntities |> listMap create)
        | _ when entity.IsArrayType -> failwith "todo"
        | _ when entity.IsNamespace -> failwith "todo"
        //| _ when
        | _ -> failwith "???"

    let rec printResults (indent: string) (entity: FSharpEntity) =
        let indent1 = $"{indent}\t"
        let indent2 = $"{indent}\t\t"

        [ yield $"{indent}Entry"
          yield $"{indent1}Namespace: {entity.Namespace}"
          yield $"{indent1}Display name: {entity.DisplayName}"
          yield $"{indent1}Members: {entity.MembersFunctionsAndValues.Count}"
          match entity with
          | _ when entity.IsFSharpModule ->

              ()
          | _ when entity.IsFSharpRecord ->
              yield!
                  entity.FSharpFields
                  |> List.ofSeq
                  |> List.map (fun f -> $"{indent2}Field: {f.Name} ({f.FieldType})")

              yield $"{indent}* is record"
          | _ when entity.IsFSharpUnion ->
              yield!
                  entity.UnionCases
                  |> List.ofSeq
                  |> List.map
                      (fun uc ->
                          $"{uc.Name} {uc.Fields
                                       |> List.ofSeq
                                       |> List.map (fun f -> f.FieldType.ToString())
                                       |> String.concat Environment.NewLine}")

              yield $"{indent}* is union"
          | _ when entity.IsClass ->
              //entity.
              yield $"{indent}* is class"
          | _ when entity.IsArrayType -> yield $"{indent}* is array"
          | _ -> ()

          //e.NestedEntities
          yield!
              entity.MembersFunctionsAndValues
              |> List.ofSeq
              |> List.map
                  (fun m ->
                      [ yield $"{indent2}Name: {m.DisplayName}"
                        yield $"{indent2}XDoc sig: {m.XmlDocSig}"
                        //X  m.XmlDoc
                        //yield $"{indent2}XDoc: {m.XmlDoc}"
                        yield $"{indent2}Type: {m.FullType}"
                        yield!
                            m.CurriedParameterGroups
                            |> List.ofSeq
                            |> List.map
                                (fun p ->
                                    p
                                    |> List.ofSeq
                                    |> List.map (fun cp -> $"{indent2}Arg: {cp.Name} {cp.Type}"))
                            |> List.concat
                        match m.ReturnParameter.Type.IsUnresolved with
                        | true ->
                            yield
                                $"{indent2}Return {m.ReturnParameter.Type.Format(FSharpDisplayContext.Empty.WithShortTypeNames(true))}"
                        | false ->
                            yield
                                $"{indent2}Return {m.ReturnParameter.Type.Format(FSharpDisplayContext.Empty.WithShortTypeNames(true))}" ])
              |> List.concat
          yield!
              entity.NestedEntities
              |> List.ofSeq
              |> List.map (printResults indent1)
              |> List.concat ]
        
module DocumentCompiler =
    
    
    let mergeExtractions _ _ = ()
    
    ()