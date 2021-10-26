namespace Mimir.Jsonic.Net

open System
open System.Globalization
open System.IO
open System.Text.Json
open Mimir.Jsonic


[<RequireQualifiedAccess>]
module Encode =
    type private Output =
        | WriteToStream of Stream
        | WriteToBuffer of Buffers.IBufferWriter<byte>

    let private write output indent (value:JsonicValue) =
        let options = JsonWriterOptions(Indented = indent)
        use writer =
            match output with
            | WriteToStream stream -> new Utf8JsonWriter(stream, options)
            | WriteToBuffer buffer -> new Utf8JsonWriter(buffer, options)

        let rec run(value:JsonicValue) =
            match value with
            | JsonicValue.Nil ->
                writer.WriteNullValue()

            | JsonicValue.Bool value ->
                writer.WriteBooleanValue(value)

            | JsonicValue.Int value ->
                writer.WriteNumberValue(value)

            | JsonicValue.Uint value ->
                writer.WriteNumberValue(value)

            | JsonicValue.Float value ->
                writer.WriteNumberValue(value)

            | JsonicValue.String value ->
                writer.WriteStringValue(value)

            | JsonicValue.Binary bytes ->
                let base64 = Convert.ToBase64String(bytes)
                writer.WriteStringValue("base64," + base64)

            | JsonicValue.Timestamp ts ->
                writer.WriteStringValue(ts.ToString("O", CultureInfo.InvariantCulture))

            | JsonicValue.Uuid v ->
                writer.WriteStringValue(v.ToString())

            | JsonicValue.Array values ->
                writer.WriteStartArray()
                values |> Array.iter run
                writer.WriteEndArray()

            | JsonicValue.Object m ->
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
        use ms = new MemoryStream()
        write (WriteToStream ms) indent value

        ms.Position <- 0L
        use sr = new StreamReader(ms)
        sr.ReadToEnd()


[<RequireQualifiedAccess>]
module Decode =
    let decodeString (decoder:Decoder<'a>) (value:string) =
        use reader = System.Text.Json.JsonDocument.Parse(value)

        let ofPairMapped f =
            Option.ofPair
            >> Option.map f

        let rec visit path (el:JsonElement) =
            match el.ValueKind with
            | JsonValueKind.Null ->
                JsonicValue.Nil

            | JsonValueKind.True ->
                JsonicValue.Bool true

            | JsonValueKind.False ->
                JsonicValue.Bool false

            | JsonValueKind.Number ->
                // Order is important, unsigned integers first, then signed, lastly float!
                el.TryGetUInt64() |> ofPairMapped JsonicValue.Uint
                |> Option.orElseWith(fun _ ->
                    el.TryGetInt64() |> ofPairMapped JsonicValue.Int)
                |> Option.orElseWith(fun _ ->
                    el.TryGetDouble() |> ofPairMapped JsonicValue.Float)
                |> Option.defaultWith(fun _ ->
                    invalidOp "Couldn't decode JSON Number")

            | JsonValueKind.String ->
                let tryGetBase64() =

                    let prefix = "base64,"
                    let stringValue = el.GetString()
                    let index = stringValue.IndexOf(prefix)

                    if index = 0 then
                        try
                            stringValue.Substring(prefix.Length)
                            |> Convert.FromBase64String
                            |> JsonicValue.Binary
                            |> Some
                        with _ ->
                            None
                    else
                        None

                // Order is important, first types formatted as string, lastly plain strings!
                tryGetBase64()
                |> Option.orElseWith(fun _ ->
                    el.TryGetDateTimeOffset() |> ofPairMapped JsonicValue.Timestamp)
                |> Option.orElseWith(fun _ ->
                    el.TryGetGuid()
                    |> ofPairMapped JsonicValue.Uuid
                    |> Option.orElseWith(fun _ -> el.GetString() |> JsonicValue.String |> Some))
                |> Option.defaultWith(fun _ ->
                    invalidOp "Couldn't decode JSON String")

            | JsonValueKind.Array ->
                el.EnumerateArray()
                |> Seq.mapi(fun index item -> visit $"{path}.[{index}]" item)
                |> Seq.toArray
                |> JsonicValue.Array

            | JsonValueKind.Object ->
                el.EnumerateObject()
                |> Seq.map(fun p ->
                    ( p.Name
                    , visit (path + "." + p.Name) p.Value
                    ))
                |> Map.ofSeq
                |> JsonicValue.Object

            | _ ->
                invalidOp "Unsupported JSON token"

        visit "$" reader.RootElement
        |> decoder "$"

[<RequireQualifiedAccess>]
module Codec =
    let decodeString codec =
        Decode.decodeString (Codec.decoder codec)

    let encodeToString indent codec =
        Codec.encoder codec >> Encode.toString indent