module Mimir.Jsonic.Saturn


open Giraffe
open Microsoft.AspNetCore.Http
open Microsoft.FSharp.Control
open Mimir.Jsonic.Net
open System.IO
open System.Text


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
    open Saturn
    open Microsoft.Extensions.Logging

    let build (definition:ApiDefinition<'apiUnion>)
              (unwrapAndHandle:'apiUnion -> HttpHandler)
              : HttpHandler =

        router {
            post "/api" (fun next ctx ->
                let logger = ctx.GetLogger()

                task {
                    match ctx.GetQueryStringValue "def" with
                    | Ok defName ->
                        match Api.tryFindWrapper defName definition with
                        | Some api ->
                            return! unwrapAndHandle api next ctx

                        | None ->
                            logger.Log(LogLevel.Error, "Undefined API: def={defName}", defName)

                            ctx.SetStatusCode 400
                            return! text $"Undefined API: def={defName}" next ctx

                    | Error _ ->
                        logger.Log(LogLevel.Error, "Unspecified API: where is the 'def' query parameter?")

                        ctx.SetStatusCode 400
                        return! text "Unspecified API: where is the 'def' query parameter?" next ctx
                }
            )
        }


    let route (api:Api<'input, 'output>)
              (exec:'input -> 'output task) : HttpHandler =

        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = ctx.GetLogger()

                match! tryDecodeAsync api.InputCodec ctx with
                | Error e ->
                    let error = Decode.errorToString e

                    logger.Log(LogLevel.Error, "Decoder failure: {error}", error)
                    return! decodeError  $"Decoder failure: {error}" next ctx

                | Ok input ->
                    let! output = exec input
                    return! encodeAsync api.OutputCodec output next ctx
            }

    let routeCtx (api:Api<'input, 'output>)
                 (exec:HttpContext -> 'input -> 'output task) : HttpHandler =

        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let logger = ctx.GetLogger()

                match! tryDecodeAsync api.InputCodec ctx with
                | Error e ->
                    let error = Decode.errorToString e

                    logger.Log(LogLevel.Error, "Decoder failure: {error}", error)
                    return! decodeError  $"Decoder failure: {error}" next ctx

                | Ok input ->
                    let! output = exec ctx input
                    return! encodeAsync api.OutputCodec output next ctx
            }

