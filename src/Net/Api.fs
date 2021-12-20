[<RequireQualifiedAccess>]
module Mimir.Jsonic.Net.Api

open FSharp.Data
open Mimir.Jsonic
open Mimir.Jsonic.Net
open Microsoft.FSharp.Control

type CallInput =
    { ApiName:string
      Value:JsonicValue
    }

type CallOutput =
    | TextOutput of text:string
    | BinaryOutput of bin:byte array


let callAsync (callAsync: CallInput -> CallOutput task)
              (definition:ApiDefinition<'apiUnion>)
              (apiCtor:Api<'input, 'output> -> 'apiUnion)
              (input: 'input)
              : 'output task =

    let anyMapping =
        definition
        |> Api.tryFind apiCtor

    match anyMapping with
    | None ->
        failwith "API: Definition not found, is it defined?"

    | Some (apiName, api) ->
        let inputValue = Codec.encodeToValue api.InputCodec input

        task {
            let! outputValue = callAsync { ApiName=apiName; Value=inputValue }

            match outputValue with
            | TextOutput json ->
                match Codec.decodeString api.OutputCodec json with
                | Ok value ->
                    return value

                | Error e ->
                    return failwithf "API: Couldn't decode http response, is your definition current?\n%s\n%s" (Decode.errorToString e) json

            | BinaryOutput _ ->
                return failwithf "API: Unsupported http response, binary data not yet supported."
        }


let jsonOverHttp (apiUrl: string)
                 (input:CallInput)
                 : CallOutput task =

    task {
        let json = Encode.toString false input.Value
        let! response =
            Http.AsyncRequest( apiUrl + "/api"
                             , query=[ "def", input.ApiName ]
                             , httpMethod="POST"
                             , body=TextRequest json
                             )

        match response.Body with
        | Text outputJson -> return TextOutput outputJson
        | Binary bin -> return BinaryOutput bin
    }
