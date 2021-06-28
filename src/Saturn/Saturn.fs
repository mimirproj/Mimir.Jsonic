module Mimir.Jsonic.Saturn

open System.IO
open System.Text
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2
open Giraffe

open Mimir.Jsonic.Net


let encodeAsync (codec:Codec<'a>) (data:'a) : HttpHandler =
    fun (_ : HttpFunc) (ctx : HttpContext) ->
        ctx.SetContentType "application/json; charset=utf-8"
        let jsonText = Codec.encodeToString false codec data
        let jsonBytes = Encoding.UTF8.GetBytes jsonText
        ctx.WriteBytesAsync jsonBytes

let tryDecodeAsync (codec:Codec<'a>) (ctx : HttpContext) =
    task {
        use reader = new StreamReader(ctx.Request.Body, Encoding.UTF8)
        let! bodyText = reader.ReadToEndAsync()

        return Codec.decodeString codec bodyText
    }

let decodeError errorText next (ctx : HttpContext) =
    ctx.SetStatusCode 400
    text errorText next ctx


[<RequireQualifiedAccess>]
module Api =
    open Saturn.Router

    let build (definition:ApiDefinition<'apiUnion>)
              (unwrapAndHandle:'apiUnion -> HttpHandler)
              : HttpHandler =

        router {
            post "/api" (fun next ctx ->
                task {
                    match ctx.GetQueryStringValue "def" with
                    | Ok defName ->
                        match Api.tryFindWrapper defName definition with
                        | Some api ->
                            return! unwrapAndHandle api next ctx

                        | None ->
                            ctx.SetStatusCode 400
                            return! text "Undefined API" next ctx

                    | Error _ ->
                        ctx.SetStatusCode 400
                        return! text "Unspecified API" next ctx
                }
            )
        }


    let route (api:Api<'input, 'output>)
              (exec:'input -> 'output task) : HttpHandler =

        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                match! tryDecodeAsync api.InputCodec ctx with
                | Error _ ->
                    return! decodeError "Decoder failure" next ctx

                | Ok input ->
                    let! output = exec input
                    return! encodeAsync api.OutputCodec output next ctx
            }

