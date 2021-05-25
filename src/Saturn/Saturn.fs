module Mimir.Jsonic.Saturn

open System.IO
open System.Text
open Microsoft.AspNetCore.Http
open Microsoft.Net.Http.Headers
open FSharp.Control.Tasks.V2
open Giraffe


open Mimir.Jsonic
open Mimir.Jsonic.Net

let encode (codec:Codec<'a>) (data:'a) : HttpHandler =
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