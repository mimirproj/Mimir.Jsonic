[<RequireQualifiedAccess>]
module Mimir.Jsonic.Net.Api

open FSharp.Data
open Mimir.Jsonic
open Mimir.Jsonic.Net

let callAsync (definition:ApiDefinition<'apiUnion>)
              (apiUrl: string)
              (apiCtor:Api<'input, 'output> -> 'apiUnion)
              (input: 'input) : Async<'output> =

    let anyMapping =
        definition
        |> Api.tryFind apiCtor

    match anyMapping with
    | None ->
        failwith "API: Definition not found, is it defined?"

    | Some (apiName, api) ->
        let inputJson = Codec.encodeToString false api.InputCodec input

        async {
            let! response =
                Http.AsyncRequest( apiUrl + "/api"
                                 , query=[ "def", apiName ]
                                 , httpMethod="POST"
                                 , body=TextRequest inputJson
                                 )

            match response.Body with
            | Text outputJson ->
                match Codec.decodeString api.OutputCodec outputJson with
                | Ok value ->
                    return value

                | Error e ->
                    return failwithf "API: Couln't decode http response, is your definition current? %A" e

            | _ ->
                return failwithf "API: Unsupported http response, text expected."
        }

