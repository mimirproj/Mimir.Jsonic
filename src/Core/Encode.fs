[<RequireQualifiedAccess>]
module Mimir.Jsonic.Encode

let unit : Encoder<unit> =
    fun _ -> JsonicValue.Nil


let bool : Encoder<_> =
    JsonicValue.Bool


let int8 : Encoder<int8> =
    int64 >> JsonicValue.Int


let uint8 : Encoder<uint8> =
    uint64 >> JsonicValue.Uint


/// Alias for Encoder.uint8
let byte : Encoder<byte> = uint8


let int16 : Encoder<int16> =
    int64 >> JsonicValue.Int


let uint16 : Encoder<uint16> =
    uint64 >> JsonicValue.Uint


let int32 : Encoder<int32> =
    int64 >> JsonicValue.Int

/// Alias for Encoder.int32
let int : Encoder<int> = int32


let uint32 : Encoder<uint32> =
    uint64 >> JsonicValue.Uint

let int64 : Encoder<int64> =
    JsonicValue.Int

let uint64 : Encoder<uint64> =
    JsonicValue.Uint

let float32 : Encoder<float32> =
    double >> JsonicValue.Float

let float64 : Encoder<float64> =
    JsonicValue.Float

let string : Encoder<string> =
    JsonicValue.String

let binary : Encoder<byte array> =
    JsonicValue.Binary

let timestamp : Encoder<System.DateTimeOffset> =
    JsonicValue.Timestamp

let uuid : Encoder<System.Guid> =
    JsonicValue.Uuid

let guid = uuid

let array : Encoder<JsonicValue array> =
    JsonicValue.Array

let seq : Encoder<JsonicValue seq> =
    Seq.toArray >> JsonicValue.Array

let list : Encoder<_> =
    List.toArray >> JsonicValue.Array



let option (encode:Encoder<'a>) =
    Option.map encode
    >> Option.defaultWith unit

let object : Encoder<(string * JsonicValue) list> =
    Map.ofList >> JsonicValue.Object

let dict : Encoder<Map<string, JsonicValue>> =
    JsonicValue.Object

let tuple2 (encodeA : Encoder<'a>)
           (encodeB : Encoder<'b>)
           (a, b)
           : JsonicValue =

    array
        [| encodeA a
           encodeB b
        |]

let tuple3 (encodeA : Encoder<'a>)
           (encodeB : Encoder<'b>)
           (encodeC : Encoder<'c>)
           (a, b, c)
           : JsonicValue =

    array
        [| encodeA a
           encodeB b
           encodeC c
        |]

let tuple4 (encodeA : Encoder<'a>)
           (encodeB : Encoder<'b>)
           (encodeC : Encoder<'c>)
           (encodeD : Encoder<'d>)
           (a, b, c, d)
           : JsonicValue =

    array
        [| encodeA a
           encodeB b
           encodeC c
           encodeD d
        |]

let tuple5 (encodeA : Encoder<'a>)
           (encodeB : Encoder<'b>)
           (encodeC : Encoder<'c>)
           (encodeD : Encoder<'d>)
           (encodeE : Encoder<'e>)
           (a, b, c, d, e)
           : JsonicValue =

    array
        [| encodeA a
           encodeB b
           encodeC c
           encodeD d
           encodeE e
        |]

let tuple6 (encodeA : Encoder<'a>)
           (encodeB : Encoder<'b>)
           (encodeC : Encoder<'c>)
           (encodeD : Encoder<'d>)
           (encodeE : Encoder<'e>)
           (encodeF : Encoder<'f>)
           (a, b, c, d, e, f)
           : JsonicValue =

    array
        [| encodeA a
           encodeB b
           encodeC c
           encodeD d
           encodeE e
           encodeF f
        |]

let tuple7 (encodeA : Encoder<'a>)
           (encodeB : Encoder<'b>)
           (encodeC : Encoder<'c>)
           (encodeD : Encoder<'d>)
           (encodeE : Encoder<'e>)
           (encodeF : Encoder<'f>)
           (encodeG : Encoder<'g>)
           (a, b, c, d, e, f, g)
           : JsonicValue =

    array
        [| encodeA a
           encodeB b
           encodeC c
           encodeD d
           encodeE e
           encodeF f
           encodeG g
        |]

let tuple8 (encodeA : Encoder<'a>)
           (encodeB : Encoder<'b>)
           (encodeC : Encoder<'c>)
           (encodeD : Encoder<'d>)
           (encodeE : Encoder<'e>)
           (encodeF : Encoder<'f>)
           (encodeG : Encoder<'g>)
           (encodeH : Encoder<'h>)
           (a, b, c, d, e, f, g, h)
           : JsonicValue =

    array
        [| encodeA a
           encodeB b
           encodeC c
           encodeD d
           encodeE e
           encodeF f
           encodeG g
           encodeH h
        |]
