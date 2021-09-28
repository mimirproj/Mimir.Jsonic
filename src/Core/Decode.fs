[<RequireQualifiedAccess>]
module Mimir.Jsonic.Decode

let inline private overflow path type' value =
        ValueOverflow {| Path = path; ValueType = type'; Actual = value |}

let inline private unexpected path type' value =
    UnexpectedType {| Path = path; ValueType = type'; Actual = value |}

let inline private badPath path fields value =
    BadPath {| Path=path; Fields=fields; Actual=value |}


(* RUNNERS
*)

let fromValue (path : string) (decoder : Decoder<'a>) =
    fun value ->
        match decoder path value with
        | Ok success ->
            Ok success
        | Error error ->
            Error error


// NOTE: The other runners are implemented in their platform specific libraries!



(* PRIMITIVES
*)

let unit : Decoder<unit> =
    fun path value ->
        match value with
        | Primitive Nil -> Ok()
        | _ -> Error(unexpected path "null" value)

let bool : Decoder<bool> =
    fun path value ->
        match value with
        | Primitive(Bool v) -> Ok v
        | _ -> Error(unexpected path "bool" value)


let inline private fromInt64 (narrow: int64 -> ^m)
                             (value: int64)
                             : ^m option =

    let (min:'m) = minValue()
    let (max:'m) = maxValue()

    if isNegative value && value >= int64 min then
        Some(narrow value)

    elif value <= int64 max then
        Some(narrow value)

    else
        None


let inline private fromUint64 (narrow: uint64 -> ^n)
                              (value: uint64)
                              : ^n option =

    let (max:'n) = maxValue()

    // We only need to check that the value is <= to maxValue 'n because input is always positive
    if value <= uint64 max then
        Some(narrow value)

    else
        None


let int8 : Decoder<int8> =
    fun path value ->
        match value with
        | Primitive(Integer(SignedInteger i)) ->
            fromInt64 int8 i
            |> Result.ofOption (overflow path "int8" value)

        | Primitive(Integer(UnsignedInteger i)) ->
            fromUint64 int8 i
            |> Result.ofOption (overflow path "int8" value)

        | _ -> Error(unexpected path "int8" value)


let uint8 : Decoder<uint8> =
    fun path value ->
        match value with
        | Primitive(Integer(SignedInteger i)) ->
            fromInt64 uint8 i
            |> Result.ofOption (overflow path "uint8" value)

        | Primitive(Integer(UnsignedInteger i)) ->
            fromUint64 uint8 i
            |> Result.ofOption (overflow path "uint8" value)

        | _ -> Error(unexpected path "uint8" value)

/// Alias to Decode.uint8
let byte : Decoder<byte> = uint8


let int16 : Decoder<int16> =
    fun path value ->
        match value with
        | Primitive(Integer(SignedInteger i)) ->
            fromInt64 int16 i
            |> Result.ofOption (overflow path "int16" value)

        | Primitive(Integer(UnsignedInteger i)) ->
            fromUint64 int16 i
            |> Result.ofOption (overflow path "int16" value)

        | _ -> Error(unexpected path "int16" value)


let uint16 : Decoder<uint16> =
    fun path value ->
        match value with
        | Primitive(Integer(SignedInteger i)) ->
            fromInt64 uint16 i
            |> Result.ofOption (overflow path "uint16" value)

        | Primitive(Integer(UnsignedInteger i)) ->
            fromUint64 uint16 i
            |> Result.ofOption (overflow path "uint16" value)

        | _ -> Error(unexpected path "uint16" value)


let int32 : Decoder<int32> =
    fun path value ->
        match value with
        | Primitive(Integer(SignedInteger i)) ->
            fromInt64 int32 i
            |> Result.ofOption (overflow path "int32" value)

        | Primitive(Integer(UnsignedInteger i)) ->
            fromUint64 int32 i
            |> Result.ofOption (overflow path "int32" value)

        | _ -> Error(unexpected path "int32" value)

/// Alias to Decode.int32
let int : Decoder<int> = int32


let uint32 : Decoder<uint32> =
    fun path value ->
        match value with
        | Primitive(Integer(SignedInteger i)) ->
            fromInt64 uint32 i
            |> Result.ofOption (overflow path "uint32" value)

        | Primitive(Integer(UnsignedInteger i)) ->
            fromUint64 uint32 i
            |> Result.ofOption (overflow path "uint32" value)

        | _ -> Error(unexpected path "uint32" value)


let int64 : Decoder<int64> =
    fun path value ->
        match value with
        | Primitive(Integer(SignedInteger i)) ->
            Ok i

        | Primitive(Integer(UnsignedInteger i)) ->
            if i <= uint64 System.Int64.MaxValue then
                Ok(int64 i)
            else
                Error(overflow path "int64" value)

        | _ -> Error(unexpected path "int64" value)

let uint64 : Decoder<uint64> =
    fun path value ->
        match value with
        | Primitive(Integer(SignedInteger i)) ->
            if i >= 0L then
                Ok(uint64 i)
            else
                Error(overflow path "uint64" value)

        | Primitive(Integer(UnsignedInteger i)) ->
            Ok i

        | _ -> Error(unexpected path "uint64" value)


let float32 : Decoder<float32> =
    fun path value ->
        match value with
        | Primitive(Float f) ->
            Ok(float32 f)

        | _ -> Error(unexpected path "float32" value)


let float64 : Decoder<float64> =
    fun path value ->
        match value with
        | Primitive(Float f) ->
            Ok f

        | _ -> Error(unexpected path "float64" value)


let string : Decoder<string> =
    fun path value ->
        match value with
        | Primitive(String s) ->
            Ok s

        | _ -> Error(unexpected path "string" value)


let binary : Decoder<byte array> =
    fun path value ->
        match value with
        | Primitive(Binary v) ->
            Ok v

        | _ -> Error(unexpected path "byte array" value)


let timestamp : Decoder<System.DateTimeOffset> =
    fun path value ->
        match value with
        | Primitive(Timestamp v) ->
            Ok v

        | _ -> Error(unexpected path "timestamp" value)


let uuid : Decoder<System.Guid> =
    fun path value ->
        match value with
        | Primitive(Uuid v) ->
            Ok v

        | _ -> Error(unexpected path "uuid" value)
        

(* OBJECT PRIMITIVES
*)

let private decodeMaybeNull path (decoder : Decoder<'a>) value =
    // The decoder may be an option decoder so give it an opportunity to check null values
    match decoder path value with
    | Ok v -> Ok(Some v)
    | Error _ when value = Primitive Nil -> Ok None
    | Error er -> Error er


let optional (fieldName : string) (decoder : Decoder<'value>) : Decoder<'value option> =
    fun path value ->
        match value with
        | Object m ->
            match m with
            | Key fieldName fieldValue ->
                decodeMaybeNull (path + "." + fieldName) decoder fieldValue

            | _ -> // Undefined
                Ok None

        | _ ->
            Error(unexpected path "object" value)


let optionalAt (fieldNames : string list) (decoder : Decoder<'value>) : Decoder<'value option> =
    fun firstPath firstValue ->
        ((firstPath, firstValue, None), fieldNames)
        ||> List.fold (fun (curPath, curValue, res) field ->
            match res with
            | Some _ -> curPath, curValue, res
            | None ->
                match curValue with
                | Primitive Nil ->
                    curPath, curValue, Some (Ok None)

                | Object m ->
                    let curValue =
                        match m with
                        | Key field curValue -> curValue
                        | _ -> Primitive Nil

                    curPath + "." + field, curValue, None

                | _ ->
                    let res = Error(unexpected curPath "object" curValue)
                    curPath, curValue, Some res
        )

        |> function
            | _, _, Some res -> res
            | lastPath, lastValue, None ->
                if lastValue = Primitive Nil then Ok None
                else decodeMaybeNull lastPath decoder lastValue


let field (fieldName: string) (decoder : Decoder<'a>) : Decoder<'a> =
    fun path value ->
        match value with
        | Object items ->
            match items with
            | Key fieldName fieldValue ->
                decoder (path + "." + fieldName) fieldValue

            | _ ->
                Error(BadField {| Path=path; FieldName=fieldName; Actual=value |})

        | _ ->
            Error(unexpected path "object" value)


let at (fieldNames: string list) (decoder : Decoder<'a>) : Decoder<'a> =
    fun firstPath firstValue ->
        ((firstPath, firstValue, None), fieldNames)
        ||> List.fold (fun (curPath, curValue, res) field ->
            match res with
            | Some _ -> curPath, curValue, res
            | None ->
                match curValue with
                | Primitive(Nil _) ->
                    let res = Error(badPath curPath fieldNames firstValue)
                    (curPath, curValue, Some res)

                | Object m ->
                    match m with
                    | Key field curValue ->
                        (curPath + "." + field, curValue, None)

                    | _ ->
                        let res = Error(badPath curPath fieldNames firstValue)
                        (curPath, curValue, Some res)

                | _ ->
                    let res = Error(unexpected curPath "object" curValue)
                    curPath, curValue, Some res)

        |> function
            | _, _, Some res -> res
            | lastPath, lastValue, None ->
                decoder lastPath lastValue


let index (index: uint) (decoder : Decoder<'a>) : Decoder<'a> =
    fun path value ->
        match value with
        | Array arr ->
            let intIndex = Operators.int index
            let arrPath = sprintf ".[%i]" index
            if intIndex < arr.Length then
                decoder arrPath arr.[intIndex]

            else
                IndexOutOfRange
                    {| Path = arrPath
                       Index = index
                       ArrayLength = Operators.uint arr.Length
                    |}
                |> Error

        | _ ->
            Error(unexpected path "array" value)


let option (decoder : Decoder<'a>) : Decoder<'a option> =
    fun path value ->
        match value with
        | Primitive Nil ->
            Ok None

        | _ ->
            decoder path value
            |> Result.map Some



(* DATA STRUCTURES
*)

let array (decoder : Decoder<'a>) : Decoder<'a array> =
    fun path value ->
        match value with
        | Array arr ->
            let mutable index = 0
            let mutable error = None
            let newArr = Array.zeroCreate arr.Length

            while error.IsNone && index < arr.Length do
                match decoder (sprintf ".[%i]" index) arr.[index] with
                | Ok nv ->
                    newArr.[index] <- nv
                    index <- index + 1

                | Error e ->
                    error <- Some e

            match error with
            | None -> Ok newArr
            | Some e -> Error e

        | _ ->
            Error(unexpected path "array" value)


let seq (decoder : Decoder<'a>) : Decoder<'a seq> =
    fun path value ->
        array decoder path value
        |> Result.map Array.toSeq


let list (decoder : Decoder<'a>) : Decoder<'a list> =
    fun path value ->
        array decoder path value
        |> Result.map Array.toList


let keys : Decoder<string list> =
    fun path value ->
        match value with
        | Object items ->
            items
            |> Map.toList
            |> List.map fst
            |> Ok

        | _ ->
            Error(unexpected path "object" value)


let keyValuePairs (decoder : Decoder<'a>) : Decoder<(string * 'a) list> =
    fun path value ->
        match value with
        | Object items ->
            (Ok [], Map.toList items)
            ||> List.fold(fun acc (fieldName, fieldValue) ->
                match acc with
                | Ok acc ->
                    match decoder path fieldValue with
                    | Error e ->
                        Error e

                    | Ok fv ->
                        (fieldName, fv) :: acc
                        |> Ok

                | Error _ ->
                    acc)
            |> Result.map List.rev

        | _ ->
            Error(unexpected path "object" value)


(* INCONSISTENT STRUCTURE
*)

let oneOf (decoders : Decoder<'a> list) : Decoder<'a> =
    fun path value ->
        let rec runner (decoders : Decoder<'a> list) (errors : _ list) =
            match decoders with
            | head::tail ->
                match fromValue path head value with
                | Ok v ->
                    Ok v
                | Error error -> runner tail (errors @ [error])
            | [] -> BadOneOf {| Path = path; Errors = errors |} |> Error

        runner decoders []


(* FANCY DECODING
*)

let nil (output : 'a) : Decoder<'a> =
    fun path value ->
        match value with
        | Primitive Nil -> Ok output
        | _ -> Error(unexpected path "null" value)


let value : Decoder<Value> =
    fun _ v ->
        Ok v


let succeed (output : 'a) : Decoder<'a> =
    fun _ _ ->
        Ok output


let fail (msg: string) : Decoder<'a> =
    fun path _ ->
        Failure {| Path=path; Message=msg |}
        |> Error


let andThen (cb: 'a -> Decoder<'b>) (decoder : Decoder<'a>) : Decoder<'b> =
    fun path value ->
        match decoder path value with
        | Error error -> Error error
        | Ok result -> cb result path value


let all (decoders: Decoder<'a> list): Decoder<'a list> =
    fun path value ->
        let rec runner (decoders: Decoder<'a> list) (values: 'a list) =
            match decoders with
            | decoder :: tail ->
                match decoder path value with
                | Ok value -> runner tail (values @ [ value ])
                | Error error -> Error error
            | [] -> Ok values

        runner decoders []


(* MAP FUNCTIONS
*)

let map
    (ctor : 'a -> 'value)
    (d1 : Decoder<'a>) : Decoder<'value> =

    fun path value ->
        match d1 path value with
        | Ok v1 -> Ok (ctor v1)
        | Error er -> Error er

let map2
    (ctor : 'a -> 'b -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>) : Decoder<'value> =

    fun path value ->
        match d1 path value, d2 path value with
        | Ok v1, Ok v2 -> Ok (ctor v1 v2)
        | Error er,_ -> Error er
        | _,Error er -> Error er

let map3
    (ctor : 'a -> 'b -> 'c -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>) : Decoder<'value> =

    fun path value ->
        match d1 path value, d2 path value, d3 path value with
        | Ok v1, Ok v2, Ok v3 -> Ok (ctor v1 v2 v3)
        | Error er,_,_ -> Error er
        | _,Error er,_ -> Error er
        | _,_,Error er -> Error er

let map4
    (ctor : 'a -> 'b -> 'c -> 'd -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>) : Decoder<'value> =

    fun path value ->
        match d1 path value, d2 path value, d3 path value, d4 path value with
        | Ok v1, Ok v2, Ok v3, Ok v4 -> Ok (ctor v1 v2 v3 v4)
        | Error er,_,_,_ -> Error er
        | _,Error er,_,_ -> Error er
        | _,_,Error er,_ -> Error er
        | _,_,_,Error er -> Error er

let map5
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>) : Decoder<'value> =

    fun path value ->
        match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value with
        | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5 -> Ok (ctor v1 v2 v3 v4 v5)
        | Error er,_,_,_,_ -> Error er
        | _,Error er,_,_,_ -> Error er
        | _,_,Error er,_,_ -> Error er
        | _,_,_,Error er,_ -> Error er
        | _,_,_,_,Error er -> Error er

let map6
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>)
    (d6 : Decoder<'f>) : Decoder<'value> =

    fun path value ->
        match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value, d6 path value with
        | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6 -> Ok (ctor v1 v2 v3 v4 v5 v6)
        | Error er,_,_,_,_,_ -> Error er
        | _,Error er,_,_,_,_ -> Error er
        | _,_,Error er,_,_,_ -> Error er
        | _,_,_,Error er,_,_ -> Error er
        | _,_,_,_,Error er,_ -> Error er
        | _,_,_,_,_,Error er -> Error er

let map7
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>)
    (d6 : Decoder<'f>)
    (d7 : Decoder<'g>) : Decoder<'value> =

    fun path value ->
        match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value, d6 path value, d7 path value with
        | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6, Ok v7 -> Ok (ctor v1 v2 v3 v4 v5 v6 v7)
        | Error er,_,_,_,_,_,_ -> Error er
        | _,Error er,_,_,_,_,_ -> Error er
        | _,_,Error er,_,_,_,_ -> Error er
        | _,_,_,Error er,_,_,_ -> Error er
        | _,_,_,_,Error er,_,_ -> Error er
        | _,_,_,_,_,Error er,_ -> Error er
        | _,_,_,_,_,_,Error er -> Error er

let map8
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>)
    (d6 : Decoder<'f>)
    (d7 : Decoder<'g>)
    (d8 : Decoder<'h>) : Decoder<'value> =

    fun path value ->
        match d1 path value, d2 path value, d3 path value, d4 path value, d5 path value, d6 path value, d7 path value, d8 path value with
        | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6, Ok v7, Ok v8 -> Ok (ctor v1 v2 v3 v4 v5 v6 v7 v8)
        | Error er,_,_,_,_,_,_,_ -> Error er
        | _,Error er,_,_,_,_,_,_ -> Error er
        | _,_,Error er,_,_,_,_,_ -> Error er
        | _,_,_,Error er,_,_,_,_ -> Error er
        | _,_,_,_,Error er,_,_,_ -> Error er
        | _,_,_,_,_,Error er,_,_ -> Error er
        | _,_,_,_,_,_,Error er,_ -> Error er
        | _,_,_,_,_,_,_,Error er -> Error er


let dict (decoder : Decoder<'value>) : Decoder<Map<string, 'value>> =
        map Map.ofList (keyValuePairs decoder)



(* TUPLE DECODERS
*)

let tuple2 (decoder1: Decoder<'T1>) (decoder2: Decoder<'T2>) : Decoder<'T1 * 'T2> =

    index 0u decoder1
    |> andThen (fun v1 ->
        index 1u decoder2
        |> andThen (fun v2 ->
            succeed (v1, v2)
        )
    )

let tuple3 (decoder1: Decoder<'T1>)
           (decoder2: Decoder<'T2>)
           (decoder3: Decoder<'T3>) : Decoder<'T1 * 'T2 * 'T3> =

    index 0u decoder1
    |> andThen (fun v1 ->
        index 1u decoder2
        |> andThen (fun v2 ->
            index 2u decoder3
            |> andThen (fun v3 ->
                succeed (v1, v2, v3)
            )
        )
    )

let tuple4 (decoder1: Decoder<'T1>)
           (decoder2: Decoder<'T2>)
           (decoder3: Decoder<'T3>)
           (decoder4: Decoder<'T4>) : Decoder<'T1 * 'T2 * 'T3 * 'T4> =

    index 0u decoder1
    |> andThen (fun v1 ->
        index 1u decoder2
        |> andThen (fun v2 ->
            index 2u decoder3
            |> andThen (fun v3 ->
                index 3u decoder4
                |> andThen (fun v4 ->
                    succeed (v1, v2, v3, v4)
                )
            )
        )
    )

let tuple5 (decoder1: Decoder<'T1>)
           (decoder2: Decoder<'T2>)
           (decoder3: Decoder<'T3>)
           (decoder4: Decoder<'T4>)
           (decoder5: Decoder<'T5>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5> =

    index 0u decoder1
    |> andThen (fun v1 ->
        index 1u decoder2
        |> andThen (fun v2 ->
            index 2u decoder3
            |> andThen (fun v3 ->
                index 3u decoder4
                |> andThen (fun v4 ->
                    index 4u decoder5
                    |> andThen (fun v5 ->
                        succeed (v1, v2, v3, v4, v5)
                    )
                )
            )
        )
    )

let tuple6 (decoder1: Decoder<'T1>)
           (decoder2: Decoder<'T2>)
           (decoder3: Decoder<'T3>)
           (decoder4: Decoder<'T4>)
           (decoder5: Decoder<'T5>)
           (decoder6: Decoder<'T6>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> =

    index 0u decoder1
    |> andThen (fun v1 ->
        index 1u decoder2
        |> andThen (fun v2 ->
            index 2u decoder3
            |> andThen (fun v3 ->
                index 3u decoder4
                |> andThen (fun v4 ->
                    index 4u decoder5
                    |> andThen (fun v5 ->
                        index 5u decoder6
                        |> andThen (fun v6 ->
                            succeed (v1, v2, v3, v4, v5, v6)
                        )
                    )
                )
            )
        )
    )

let tuple7 (decoder1: Decoder<'T1>)
           (decoder2: Decoder<'T2>)
           (decoder3: Decoder<'T3>)
           (decoder4: Decoder<'T4>)
           (decoder5: Decoder<'T5>)
           (decoder6: Decoder<'T6>)
           (decoder7: Decoder<'T7>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7> =

    index 0u decoder1
    |> andThen (fun v1 ->
        index 1u decoder2
        |> andThen (fun v2 ->
            index 2u decoder3
            |> andThen (fun v3 ->
                index 3u decoder4
                |> andThen (fun v4 ->
                    index 4u decoder5
                    |> andThen (fun v5 ->
                        index 5u decoder6
                        |> andThen (fun v6 ->
                            index 6u decoder7
                            |> andThen (fun v7 ->
                                succeed (v1, v2, v3, v4, v5, v6, v7)
                            )
                        )
                    )
                )
            )
        )
    )

let tuple8 (decoder1: Decoder<'T1>)
           (decoder2: Decoder<'T2>)
           (decoder3: Decoder<'T3>)
           (decoder4: Decoder<'T4>)
           (decoder5: Decoder<'T5>)
           (decoder6: Decoder<'T6>)
           (decoder7: Decoder<'T7>)
           (decoder8: Decoder<'T8>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8> =

    index 0u decoder1
    |> andThen (fun v1 ->
        index 1u decoder2
        |> andThen (fun v2 ->
            index 2u decoder3
            |> andThen (fun v3 ->
                index 3u decoder4
                |> andThen (fun v4 ->
                    index 4u decoder5
                    |> andThen (fun v5 ->
                        index 5u decoder6
                        |> andThen (fun v6 ->
                            index 6u decoder7
                            |> andThen (fun v7 ->
                                index 7u decoder8
                                |> andThen (fun v8 ->
                                    succeed (v1, v2, v3, v4, v5, v6, v7, v8)
                                )
                            )
                        )
                    )
                )
            )
        )
    )



(* OBJECT BUILDER
*)

type IRequiredGetter =
    abstract Field : string -> Decoder<'a> -> 'a
    abstract At : List<string> -> Decoder<'a> -> 'a
    //abstract Raw : Decoder<'a> -> 'a

type IOptionalGetter =
    abstract Field : string -> Decoder<'a> -> 'a option
    abstract At : List<string> -> Decoder<'a> -> 'a option
    //abstract Raw : Decoder<'a> -> 'a option

type IGetters =
    abstract Required: IRequiredGetter
    abstract Optional: IOptionalGetter

let private unwrapWith (errors: ResizeArray<Error>) path (decoder: Decoder<'T>) value: 'T =
    match decoder path value with
    | Ok v -> v
    | Error er ->
        errors.Add(er);
        Unchecked.defaultof<'T>


type Getters<'T>(path: string, v: Value) =
    let mutable errors = ResizeArray<Error>()

    let required =
        { new IRequiredGetter with
            member __.Field (fieldName : string) (decoder : Decoder<_>) =
                unwrapWith errors path (field fieldName decoder) v

            member __.At (fieldNames : string list) (decoder : Decoder<_>) =
                unwrapWith errors path (at fieldNames decoder) v

            // member __.Raw (decoder: Decoder<_>) =
            //     unwrapWith errors path decoder v
        }

    let optional =
        { new IOptionalGetter with
            member __.Field (fieldName : string) (decoder : Decoder<_>) =
                unwrapWith errors path (optional fieldName decoder) v

            member __.At (fieldNames : string list) (decoder : Decoder<_>) =
                unwrapWith errors path (optionalAt fieldNames decoder) v

            // member __.Raw (decoder: Decoder<_>) =
            //     match decoder path v with
            //     | Ok v -> Some v
            //     | Error((_, reason) as error) ->
            //         match reason with
            //         | BadPrimitive(_,v)
            //         | BadPrimitiveExtra(_,v,_)
            //         | BadType(_,v) ->
            //             if Helpers.isNullValue v then None
            //             else errors.Add(error); Unchecked.defaultof<_>
            //         | BadField _
            //         | BadPath _ -> None
            //         | TooSmallArray _
            //         | FailMessage _
            //         | BadOneOf _ -> errors.Add(error); Unchecked.defaultof<_>
        }

    member __.Errors: _ list =
        Seq.toList errors

    interface IGetters with
        member __.Required = required
        member __.Optional = optional

let object (builder: IGetters -> 'value) : Decoder<'value> =
    fun path v ->
        let getters = Getters(path, v)
        let result = builder getters

        match getters.Errors with
        | [] ->
            Ok result

        | fst :: _ as errors ->
            if errors.Length > 1 then
                BadOneOf
                    {| Path = path
                       Errors = errors
                    |}
                |> Error

            else
                Error fst