namespace Mimir.Jsonic

open System

type float64 = Double

[<RequireQualifiedAccess>]
type JsonicValue =
    | Nil
    | Bool      of bool
    | Int       of int64
    | Uint      of uint64
    | Float     of float64
    | String    of string
    | Binary    of byte array
    | Timestamp of DateTimeOffset
    | Uuid      of Guid
    | Array     of JsonicValue array
    | Object    of Map<string, JsonicValue>


type Encoder<'a> = 'a -> JsonicValue
type Decoder<'a> = string -> JsonicValue -> Result<'a, JsonicError>

and [<RequireQualifiedAccess>]
    JsonicError =
    | Failure of {| Path:string; Message:string |}
    | BadOneOf of {| Path:string; Errors: JsonicError list |}
    | IndexOutOfRange of {| Path:string; Index: uint; ArrayLength: uint |}
    | TypeMismatch of {| Path:string; ValueType:string; Actual: JsonicValue |}
    | FieldMissing of {| Path:string; FieldName:string; Actual: JsonicValue |}
    | ValueOverflow of {| Path:string; ValueType:string; Actual: JsonicValue |}
    | InvalidPath of {| Path:string; Fields:string list; Actual: JsonicValue |}
