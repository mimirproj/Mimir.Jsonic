namespace Mimir.Jsonic.Net

open System
open System.Text
open System.Globalization
open System.IO
open Mimir.Jsonic

open Newtonsoft.Json
open Newtonsoft.Json.Linq

[<RequireQualifiedAccess>]
module Encode =
    let private write (output:StringBuilder) indent (value:Value) =
        use sw = new StringWriter(output)
        use writer = new JsonTextWriter(sw)
        writer.Formatting <-
            if indent then Formatting.Indented
            else Formatting.None

        let rec run(value:Value) =
            match value with
            | Primitive prim ->
                match prim with
                | Nil ->
                    writer.WriteNull()

                | Bool value ->
                    writer.WriteValue(value)

                | Integer iv ->
                    match iv with
                    | SignedInteger value ->
                        writer.WriteValue(value)

                    | UnsignedInteger value ->
                        writer.WriteValue(value)

                | Float value ->
                    writer.WriteValue(value)

                | String value ->
                    writer.WriteValue(value)

                | Binary bytes ->
                    let base64 = Convert.ToBase64String(bytes)
                    writer.WriteValue("base64," + base64)

                | Timestamp ts ->
                    writer.WriteValue(ts.ToString("O", CultureInfo.InvariantCulture))

            | Array values ->
                writer.WriteStartArray()
                values |> Array.iter run
                writer.WriteEndArray()

            | Object m ->
                writer.WriteStartObject()

                Map.toArray m
                |> Array.iter (fun (fieldName, fieldValue) ->
                    writer.WritePropertyName(fieldName)
                    run fieldValue
                )

                writer.WriteEndObject()
            ()

        run value

    let toString indent value =
        let sb = StringBuilder()
        write sb indent value

        sb.ToString()


[<RequireQualifiedAccess>]
module Decode =
    module private Helpers =
        let anyToString (token: JToken) : string =
            if isNull token then "null"
            else
                use stream = new StringWriter(NewLine = "\n")
                use jsonWriter = new JsonTextWriter(
                                        stream,
                                        Formatting = Formatting.Indented,
                                        Indentation = 4 )

                token.WriteTo(jsonWriter)
                stream.ToString()

        let inline getField (fieldName: string) (token: JToken) = token.Item(fieldName)
        let inline isBool (token: JToken) = not(isNull token) && token.Type = JTokenType.Boolean
        let inline isNumber (token: JToken) = not(isNull token) && (token.Type = JTokenType.Float || token.Type = JTokenType.Integer)
        let inline isIntegralValue (token: JToken) = not(isNull token) && (token.Type = JTokenType.Integer)
        let inline isInteger (token: JToken) = not(isNull token) && (token.Type = JTokenType.Integer)
        let inline isString (token: JToken) = not(isNull token) && token.Type = JTokenType.String
        let inline isGuid (token: JToken) = not(isNull token) && token.Type = JTokenType.Guid
        let inline isDate (token: JToken) = not(isNull token) && token.Type = JTokenType.Date
        let inline isArray (token: JToken) = not(isNull token) && token.Type = JTokenType.Array
        let inline isObject (token: JToken) = not(isNull token) && token.Type = JTokenType.Object
        let inline isUndefined (token: JToken) = isNull token
        let inline isNullValue (token: JToken) = isNull token || token.Type = JTokenType.Null
        let inline asBool (token: JToken): bool = token.Value<bool>()
        let inline asInt (token: JToken): int = token.Value<int>()
        let inline asFloat (token: JToken): float = token.Value<float>()
        let inline asFloat32 (token: JToken): float32 = token.Value<float32>()
        let inline asDecimal (token: JToken): System.Decimal = token.Value<System.Decimal>()
        let inline asString (token: JToken): string = token.Value<string>()
        let inline asArray (token: JToken): JToken[] = token.Value<JArray>().Values() |> Seq.toArray


    let decodeString (decoder:Decoder<'a>) (value:string) =
        let ofPairMapped f =
            Option.ofPair
            >> Option.map f

        let rec visit path (el:JToken) =
            match el.Type with
            | JTokenType.Null ->
                Primitive Nil

            | JTokenType.Boolean ->
                let v = el.Value<_>()
                Primitive(Bool v)

            | JTokenType.Float  ->
                let v = el.Value<_>()
                Primitive(Float v)

            | JTokenType.Integer ->
                let v = el.ToString()

                // Order is important, unsigned integers first, then signed!
                UInt64.TryParse(v) |> ofPairMapped (UnsignedInteger >> Integer >> Primitive)
                |> Option.orElseWith(fun _ ->
                    Int64.TryParse(v) |> ofPairMapped (SignedInteger >> Integer >> Primitive))
                |> Option.defaultWith(fun _ ->
                    invalidOp "Couldn't decode JSON Number")

            | JTokenType.String ->
                let tryGetBase64() =

                    let prefix = "base64,"
                    let stringValue = el.Value<string>()
                    let index = stringValue.IndexOf(prefix)

                    if index = 0 then
                        try
                            stringValue.Substring(prefix.Length)
                            |> Convert.FromBase64String
                            |> Binary |> Primitive
                            |> Some
                        with _ ->
                            None
                    else
                        None

                // Order is important, first types formatted as string, lastly plain strings!
                tryGetBase64()
                |> Option.orElseWith(fun _ ->
                    let v = el.Value<string>()
                    DateTimeOffset.TryParse(v) |> ofPairMapped (Timestamp >> Primitive))
                |> Option.orElseWith(fun _ ->
                    el.Value<string>() |> String |> Primitive |> Some)
                |> Option.defaultWith(fun _ ->
                    invalidOp "Couldn't decode JSON String")

            | JTokenType.Array ->
                Helpers.asArray el
                |> Seq.mapi(fun index item -> visit $"{path}.[{index}]" item)
                |> Seq.toArray
                |> Array

            | JTokenType.Object ->
                let v = el.Value<JObject>()
                v.Properties()
                |> Seq.map(fun p ->
                    ( p.Name
                    , visit (path + "." + p.Name) p.Value
                    ))
                |> Map.ofSeq
                |> Object

            | _ ->
                invalidOp "Unsupported JSON token"

        JToken.Parse(value)
        |> visit "$"
        |> decoder "$"

[<RequireQualifiedAccess>]
module Codec =
    let decodeString codec =
        Decode.decodeString (Codec.decoder codec)

    let encodeToString indent codec =
        Codec.encoder codec >> Encode.toString indent