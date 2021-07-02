module Mimir.Jsonic.Net.Test.Program

open Mimir.Jsonic
open Mimir.Jsonic.Net

type SubType =
    { Bla: string
    }

let subTypeCodec =
    Codec.object (fun a ->
        { Bla = a
        })
    |> Codec.field "bla" (fun v -> v.Bla) Codec.string
    |> Codec.buildObject

type Test =
    { SomeBool: bool
      SomeString: string

      SomeInt8Low: int8
      SomeInt8High: int8
      SomeUint8Low: uint8
      SomeUint8High: uint8

      SomeInt16Low: int16
      SomeInt16High: int16
      SomeUint16Low: uint16
      SomeUint16High: uint16

      SomeInt32Low: int32
      SomeInt32High: int32
      SomeUint32Low: uint32
      SomeUint32High: uint32

      SomeInt64Low: int64
      SomeInt64High: int64
      SomeUint64Low: uint64
      SomeUint64High: uint64

      SomeFloat32Low: float32
      SomeFloat32High: float32

      SomeFloat64Low: float64
      SomeFloat64High: float64

      SomeBinary: byte []

      SomeSubType: SubType

      SomeList: string list
    }

let testCodec =
    Codec.object (fun a b c d e f g h i j k l m n o p q r s t u v w x y ->
        { SomeBool = a
          SomeString = b

          SomeInt8Low = c
          SomeInt8High = d
          SomeUint8Low = e
          SomeUint8High = f

          SomeInt16Low = g
          SomeInt16High = h
          SomeUint16Low = i
          SomeUint16High = j

          SomeInt32Low = k
          SomeInt32High = l
          SomeUint32Low = m
          SomeUint32High = n

          SomeInt64Low = o
          SomeInt64High = p
          SomeUint64Low = q
          SomeUint64High = r

          SomeFloat32Low = s
          SomeFloat32High = t

          SomeFloat64Low = u
          SomeFloat64High = v

          SomeBinary = w

          SomeSubType = x

          SomeList = y
        })
    |> Codec.field "someBool" (fun v -> v.SomeBool) Codec.bool
    |> Codec.field "someString" (fun v -> v.SomeString) Codec.string
    |> Codec.field "someInt8Low" (fun v -> v.SomeInt8Low) Codec.int8
    |> Codec.field "someInt8High" (fun v -> v.SomeInt8High) Codec.int8
    |> Codec.field "someUint8Low" (fun v -> v.SomeUint8Low) Codec.uint8
    |> Codec.field "someUint8High" (fun v -> v.SomeUint8High) Codec.uint8
    |> Codec.field "someInt16Low" (fun v -> v.SomeInt16Low) Codec.int16
    |> Codec.field "someInt16High" (fun v -> v.SomeInt16High) Codec.int16
    |> Codec.field "someUint16Low" (fun v -> v.SomeUint16Low) Codec.uint16
    |> Codec.field "someUint16High" (fun v -> v.SomeUint16High) Codec.uint16
    |> Codec.field "someInt32Low" (fun v -> v.SomeInt32Low) Codec.int32
    |> Codec.field "someInt32High" (fun v -> v.SomeInt32High) Codec.int32
    |> Codec.field "someUint32Low" (fun v -> v.SomeUint32Low) Codec.uint32
    |> Codec.field "someUint32High" (fun v -> v.SomeUint32High) Codec.uint32
    |> Codec.field "someInt64Low" (fun v -> v.SomeInt64Low) Codec.int64
    |> Codec.field "someInt64High" (fun v -> v.SomeInt64High) Codec.int64
    |> Codec.field "someUint64Low" (fun v -> v.SomeUint64Low) Codec.uint64
    |> Codec.field "someUint64High" (fun v -> v.SomeUint64High) Codec.uint64
    |> Codec.field "someFloat32Low" (fun v -> v.SomeFloat32Low) Codec.float32
    |> Codec.field "someFloat32High" (fun v -> v.SomeFloat32High) Codec.float32
    |> Codec.field "someFloat64Low" (fun v -> v.SomeFloat64Low) Codec.float64
    |> Codec.field "someFloat64High" (fun v -> v.SomeFloat64High) Codec.float64
    |> Codec.field "someBinary" (fun v -> v.SomeBinary) Codec.binary
    |> Codec.field "someSubType" (fun v -> v.SomeSubType) subTypeCodec
    |> Codec.field "someList" (fun v -> v.SomeList) (Codec.list Codec.string)
    |> Codec.buildObject

let test () =
    let valueIn =
        { SomeBool = true
          SomeString = "Hello"
          SomeUint8Low = minValue()
          SomeUint8High = maxValue()
          SomeInt8Low = minValue()
          SomeInt8High = maxValue()
          SomeUint16Low = minValue()
          SomeUint16High = maxValue()
          SomeInt16Low = minValue()
          SomeInt16High = maxValue()
          SomeUint32Low = minValue()
          SomeUint32High = maxValue()
          SomeInt32Low = minValue()
          SomeInt32High = maxValue()
          SomeUint64Low = minValue()
          SomeUint64High = maxValue()
          SomeInt64Low = minValue()
          SomeInt64High = maxValue()
          SomeFloat32Low = minValue()
          SomeFloat32High = maxValue()
          SomeFloat64Low = minValue()
          SomeFloat64High = maxValue()
          SomeBinary = [| 0uy; 1uy; 2uy; 3uy; 254uy; 255uy |]
          SomeSubType = { Bla = "Hey Bob"}
          SomeList = [ "Boo"; "Bar"; "Baz" ]
        }

    let json =
        valueIn
        |> Codec.encodeToString false testCodec

    let valueOut =
        Codec.decodeString testCodec json


    printfn "equal: %b" (Ok valueIn = valueOut)


test()