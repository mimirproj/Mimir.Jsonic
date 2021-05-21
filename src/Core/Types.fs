namespace Mimir.Jsonic

open System

type float64 = Double


type Integer =
    | SignedInteger     of int64
    | UnsignedInteger   of uint64

type Primitive =
    | Nil               of unit
    | Bool              of bool
    | Integer           of Integer
    | Float             of float64
    | String            of string
    | Binary            of byte array
    | Timestamp         of DateTimeOffset

type Value =
    | Primitive         of Primitive
    | Array             of Value array
    | Object            of Map<string, Value>


type Encoder<'a> = 'a -> Value
type Decoder<'a> = string -> Value -> Result<'a, Error>
 and Error =
    | ValueOverflow of {| Path:string; ValueType:string; Actual: Value |}
    | UnexpectedType of {| Path:string; ValueType:string; Actual: Value |}
    | IndexOutOfRange of {| Path:string; Index: uint; ArrayLength: uint |}
    | BadOneOf of {| Path:string; Errors: Error list |}
    | BadField of {| Path:string; FieldName:string; Actual: Value |}
    | Failure of {| Path:string; Message:string |}
    | BadPath of {| Path:string; Fields:string list; Actual: Value |}
    | ReaderFailure
