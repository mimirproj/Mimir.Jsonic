module Mimir.Jsonic.Encode


let nil : Encoder<_> =
    Nil >> Primitive

let bool : Encoder<_> =
    Bool >> Primitive

let int8 : Encoder<int8> =
    int64
    >> SignedInteger
    >> Integer
    >> Primitive

let uint8 : Encoder<uint8> =
    uint64
    >> UnsignedInteger
    >> Integer
    >> Primitive

let int16 : Encoder<int16> =
    int64
    >> SignedInteger
    >> Integer
    >> Primitive

let uint16 : Encoder<uint16> =
    uint64
    >> UnsignedInteger
    >> Integer
    >> Primitive

let int32 : Encoder<int32> =
    int64
    >> SignedInteger
    >> Integer
    >> Primitive

let uint32 : Encoder<uint32> =
    uint64
    >> UnsignedInteger
    >> Integer >> Primitive

let int64 : Encoder<int64> =
    SignedInteger
    >> Integer
    >> Primitive

let uint64 : Encoder<uint64> =
    UnsignedInteger
    >> Integer
    >> Primitive

let float32 : Encoder<float32> =
    double >> Float >> Primitive

let float64 : Encoder<float64> =
    Float >> Primitive

let string : Encoder<string> =
    String >> Primitive

let binary : Encoder<byte array> =
    Binary >> Primitive

let timestamp : Encoder<System.DateTimeOffset> =
    Timestamp >> Primitive

let option (encode:Encoder<'a>) =
    Option.map encode
    >> Option.defaultWith nil

let seq : Encoder<Value seq> =
    Seq.toArray >> Array

let list : Encoder<_> =
    List.toArray >> Array

let array : Encoder<Value array> =
    Array

let object : Encoder<(string * Value) list> =
    Map.ofList >> Object

let stringMap : Encoder<Map<string, Value>> =
    Object

let tuple2 (encodeA : Encoder<'a>)
           (encodeB : Encoder<'b>)
           (a, b)
           : Value =

    array
        [| encodeA a
           encodeB b
        |]

let tuple3 (encodeA : Encoder<'a>)
           (encodeB : Encoder<'b>)
           (encodeC : Encoder<'c>)
           (a, b, c)
           : Value =

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
           : Value =

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
           : Value =

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
           : Value =

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
           : Value =

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
           : Value =

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
