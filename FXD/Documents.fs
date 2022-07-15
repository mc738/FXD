namespace FXD

open System
open System.Configuration
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Text.RegularExpressions
open FDOM.Core.Common
open FDOM.Core.Parsing
open FDOM.Rendering
open FXD
open Fluff.Core

(*
[<RequireQualifiedAccess>]
module Templating =

    let slugifyName (name: string) =
        name
        |> Seq.fold
            (fun acc c ->
                match c with
                | _ when Char.IsLetterOrDigit c -> acc @ [ Char.ToLower c ]
                | _ when c = ' ' -> acc @ [ '_' ]
                | _ when c = '-' -> acc @ [ c ]
                | _ when c = '.' -> acc @ [ '-' ]
                | _ -> acc)
            []
        |> fun c -> String.Join("", c)

    let handleXmlDoc doc =
        match doc with
        | Some xdoc ->
            let summary =
                match xdoc.Summary with
                | Some s -> Some s
                | None -> None

            let returns =
                match xdoc.Returns with
                | Some s -> Some s
                | None -> None

            let examples = xdoc.Examples

            summary, examples, returns
        | None -> None, [], None

    let createClassValues (extraValues: (string * Mustache.Value) list) (cd: ClassDocument) =
        let (summary, examples, returns) =
            handleXmlDoc cd.XmlDocument

        // TODO add indexes.

        [ "id", Mustache.Value.Scalar cd.Id
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "name", Mustache.Value.Scalar cd.DisplayName
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "properties",
          Mustache.Value.Array(
              cd.Properties
              |> List.map (fun p ->
                  let (pSummary, pExamples, pReturns) =
                      handleXmlDoc p.XmlDocument

                  [ "id", Mustache.Value.Scalar p.Id
                    "className", Mustache.Scalar cd.DisplayName
                    "name", Mustache.Value.Scalar p.Name
                    "type", Mustache.Value.Scalar p.Type
                    "summary", Mustache.Value.Scalar(pSummary |> Option.defaultValue "")
                    "returns", Mustache.Value.Scalar(pReturns |> Option.defaultValue "")
                    "examples", Mustache.Value.Array(pExamples |> List.map Mustache.Value.Scalar) ]
                  |> Map.ofList
                  |> Mustache.Object)
          )
          "methods",
          Mustache.Value.Array(
              cd.Methods
              |> List.map (fun m ->
                  let (mSummary, mExamples, mReturns) =
                      handleXmlDoc m.XmlDocument

                  [ "id", Mustache.Value.Scalar m.Id
                    "className", Mustache.Scalar ""
                    "name", Mustache.Value.Scalar m.Name
                    "signature", Mustache.Value.Scalar m.Signature
                    "parameters",
                    m.Parameters
                    |> List.map (fun p ->
                        [ "name", Mustache.Value.Scalar p.Name
                          "document", Mustache.Value.Scalar(p.Document |> Option.defaultValue "")
                          "type", Mustache.Value.Scalar p.Type ]
                        |> Map.ofList
                        |> Mustache.Value.Object)
                    |> Mustache.Value.Array
                    "summary", Mustache.Value.Scalar(mSummary |> Option.defaultValue "")
                    "returns", Mustache.Value.Scalar(mReturns |> Option.defaultValue "")
                    "examples", Mustache.Value.Array(mExamples |> List.map Mustache.Value.Scalar) ]
                  |> Map.ofList
                  |> Mustache.Object)
          ) ]
        @ extraValues
        |> Map.ofList

    let createClassData (cd: ClassDocument) =
        createClassValues [] cd |> Mustache.Value.Object

    let createFunctionData (fd: FunctionDocument) =
        let (summary, examples, returns) =
            handleXmlDoc fd.XmlDocument

        [ "id", Mustache.Value.Scalar fd.Id
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "signature", Mustache.Value.Scalar fd.Signature
          "name", Mustache.Value.Scalar fd.DisplayName
          "parameters",
          Mustache.Value.Array(
              fd.Parameters
              |> List.map (fun p ->
                  [ "id", Mustache.Value.Scalar p.Id
                    "name", Mustache.Value.Scalar p.Name
                    "document", Mustache.Value.Scalar(p.Document |> Option.defaultValue "")
                    "type", Mustache.Value.Scalar p.Type ]
                  |> Map.ofList
                  |> Mustache.Object)
          ) ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createRecordData (rd: RecordDocument) =
        let (summary, examples, returns) =
            handleXmlDoc rd.XmlDocument

        [ "id", Mustache.Value.Scalar rd.Id
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "name", Mustache.Value.Scalar rd.DisplayName
          "fields",
          Mustache.Value.Array(
              rd.Fields
              |> List.map (fun f ->
                  let (fSummary, fExamples, fReturns) =
                      handleXmlDoc f.XmlDocument

                  [ "id", Mustache.Value.Scalar f.Id
                    "summary", Mustache.Value.Scalar(fSummary |> Option.defaultValue "")
                    "returns", Mustache.Value.Scalar(fReturns |> Option.defaultValue "")
                    "examples", Mustache.Value.Array(fExamples |> List.map Mustache.Value.Scalar)
                    "name", Mustache.Value.Scalar f.Name
                    "type", Mustache.Value.Scalar f.Type ]
                  |> Map.ofList
                  |> Mustache.Object)
          ) ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createUnionData (ud: UnionDocument) =
        let (summary, examples, returns) =
            handleXmlDoc ud.XmlDocument

        [ "id", Mustache.Value.Scalar ud.Id
          "summary", Mustache.Value.Scalar(summary |> Option.defaultValue "")
          "returns", Mustache.Value.Scalar(returns |> Option.defaultValue "")
          "examples", Mustache.Value.Array(examples |> List.map Mustache.Value.Scalar)
          "name", Mustache.Value.Scalar ud.DisplayName
          "members",
          Mustache.Value.Array(
              ud.Members
              |> List.map (fun f ->
                  let (fSummary, fExamples, fReturns) =
                      handleXmlDoc f.XmlDocument

                  [ "id", Mustache.Value.Scalar f.Id
                    "summary", Mustache.Value.Scalar(fSummary |> Option.defaultValue "")
                    "returns", Mustache.Value.Scalar(fReturns |> Option.defaultValue "")
                    "examples", Mustache.Value.Array(fExamples |> List.map Mustache.Value.Scalar)
                    "name", Mustache.Value.Scalar f.Name
                    "type", Mustache.Value.Scalar f.Type ]
                  |> Map.ofList
                  |> Mustache.Object)
          ) ]
        |> Map.ofList
        |> Mustache.Value.Object

    let createMemberData (m: Member) =
        match m with
        | Member.Class cd -> createClassData cd
        | Member.Function fd -> createFunctionData fd
        | Member.Module md -> failwith ""
        | Member.Namespace nd -> failwith ""
        | Member.Record rd -> createRecordData rd
        | Member.Union ud -> createUnionData ud

    let createData (members: Member list) =
        members
        |> List.fold
            (fun (cAcc, fAcc, mAcc, nAcc, rAcc, uAcc) m ->
                match m with
                | Member.Class cd -> cAcc @ [ createClassData cd ], fAcc, mAcc, nAcc, rAcc, uAcc
                | Member.Function fd -> cAcc, fAcc @ [ createFunctionData fd ], mAcc, nAcc, rAcc, uAcc
                | Member.Module md -> cAcc, fAcc, mAcc, nAcc, rAcc, uAcc
                | Member.Namespace nd -> cAcc, fAcc, mAcc, nAcc, rAcc, uAcc
                | Member.Record rd -> cAcc, fAcc, mAcc, nAcc, rAcc @ [ createRecordData rd ], uAcc
                | Member.Union ud -> cAcc, fAcc, mAcc, nAcc, rAcc, uAcc @ [ createUnionData ud ])
            ([], [], [], [], [], [])
        |> fun (cAcc, fAcc, mAcc, nAcc, rAcc, uAcc) ->
            ({ Values =
                [ "classes", Mustache.Value.Array cAcc
                  "functions", Mustache.Value.Array fAcc
                  "modules", Mustache.Value.Array mAcc
                  "namespaces", Mustache.Value.Array nAcc
                  "records", Mustache.Value.Array rAcc
                  "unions", Mustache.Value.Array uAcc ]
                |> Map.ofList
               Partials = Map.empty }: Mustache.Data)
*)