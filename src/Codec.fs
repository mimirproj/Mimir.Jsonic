namespace Mimir.Jsonic

type private CodecData<'a> =
    { encoder:Encoder<'a>
      decoder:Decoder<'a>
    }

type Codec<'a> = private Codec of CodecData<'a>


type private ObjectCodecData<'a,'b> =
    { encoder : 'a -> (string * Value) list
      decoder : Decoder<'b>
    }

/// A partially built `Codec` for an object.
type ObjectCodec<'a,'b> = private ObjectCodec of ObjectCodecData<'a,'b>


type private CustomCodecData<'encodeValue, 'v> =
    { encodeValue: 'encodeValue
      decoder: Map<string, Decoder<'v>>
    }

/// A partially built `Codec` for a custom type.
type CustomCodec<'encodeValue, 'v> = private CustomCodec of CustomCodecData<'encodeValue, 'v>

[<RequireQualifiedAccess>]
module Codec =
    (* DECODE *)

    /// Extracts the `Decoder` contained inside the `Codec`.
    let decoder (Codec m) =
        m.decoder

    // /// Parse the given string into a JSON value and then run the `Codec` on it.
    // /// This will fail if the string is not well-formed JSON or if the `Codec`
    // /// fails for some reason.
    // let decodeString codec =
    //     Decode.fromString (decoder codec)

    /// Run a `Codec` to decode some JSON `Value`.
    let decodeValue codec =
        Decode.fromValue "$" (decoder codec)




    (* ENCODE *)

    /// Extracts the encoding function contained inside the `Codec`.
    let encoder (Codec m) =
        m.encoder

    // /// Convert a value into a prettified JSON string. The first argument specifies
    // /// the amount of indentation in the result string.
    // let encodeToString indentation codec =
    //     encoder codec >> Encode.toString indentation

    /// Convert a value into a Javascript `Value`.
    let encodeToValue codec =
        encoder codec

    /// Encode.list function that works like Elm's does.
    let private listEncode encoder =
        List.map encoder
        >> Encode.list



    (* BASE *)

    /// Build your own custom `Codec`.
    /// Useful if you have pre-existing `Decoder`s you need to use.
    let build encoder decoder =
        Codec { encoder = encoder
                decoder = decoder }

    let nil =
        build Encode.nil Decode.nil

    /// `Codec` between a boolean and a `Bool`
    let bool =
        build Encode.bool Decode.bool

    /// `Codec` between a number and an `int8`
    let int8 =
        build Encode.int8 Decode.int8

    /// `Codec` between a number and an `uint8`
    let uint8 =
        build Encode.uint8 Decode.uint8

    /// `Codec` between a number and an `int16`
    let int16 =
        build Encode.int16 Decode.int16

    /// `Codec` between a number and an `uint16`
    let uint16 =
        build Encode.uint16 Decode.uint16

    /// `Codec` between a number and an `int32`
    let int32 =
        build Encode.int32 Decode.int32

    /// `Codec` between a number and an `uint32`
    let uint32 =
        build Encode.uint32 Decode.uint32

    /// `Codec` between a number and an `int64`
    let int64 =
        build Encode.int64 Decode.int64

    /// `Codec` between a number and an `uint64`
    let uint64 =
        build Encode.uint64 Decode.uint64

    /// `Codec` between a number and a `float32`
    let float32 =
        build Encode.float32 Decode.float32

    /// `Codec` between a number and a `float64`
    let float64 =
        build Encode.float64 Decode.float64

    /// `Codec` between a string and a `string`
    let string =
        build Encode.string Decode.string

    /// `Codec` between a JSON string or a MsgPack binary and a `byte array`
    let binary =
        build Encode.binary Decode.binary

    /// `Codec` between a JSON string or a MsgPack timestamp and a `DateTimeOffset`
    let timestamp =
        build Encode.timestamp Decode.timestamp



    (* CUSTOM *)


    /// Starts building a `Codec` for a custom type.
    /// You need to pass a pattern matching function, see the examples and FAQ for details.
    let custom encodeValue =
        CustomCodec { encodeValue = encodeValue
                      decoder = Map.empty
                    }


    let private variant name
                matchPiece
                decoderPiece
                (CustomCodec am) : CustomCodec<'b, 'v> =

        let enc v =
            Encode.object
                [ ( "tag", Encode.string name )
                  ( "args", listEncode id v )
                ]

        CustomCodec
            { encodeValue = am.encodeValue <| matchPiece enc
              decoder = Map.add name decoderPiece am.decoder
            }

    /// Define a variant with 0 parameters for a custom type.
    let variant0 name ctor : CustomCodec<_,'v> -> CustomCodec<'a,'v> =
        variant name
            (fun c -> c [])
            (Decode.succeed ctor)

    /// Define a variant with 1 parameters for a custom type.
    let variant1 name ctor m1 : CustomCodec<_,'v> -> CustomCodec<'b,'v> =
        variant name
            (fun c v ->
                c [ encoder m1 v ])
            (Decode.map ctor
                (Decode.index 0u <| decoder m1))

    /// Define a variant with 2 parameters for a custom type.
    let variant2 name ctor m1 m2 : CustomCodec<_,'v> -> CustomCodec<'c,'v> =
        variant name
            (fun c v1 v2 ->
                c
                    [ encoder m1 v1
                      encoder m2 v2
                    ]
            )
            (Decode.map2 ctor
                (Decode.index 0u <| decoder m1)
                (Decode.index 1u <| decoder m2)
            )

    /// Define a variant with 3 parameters for a custom type.
    let variant3 name ctor m1 m2 m3 : CustomCodec<_,'v> -> CustomCodec<'partial,'v> =
        variant name
            (fun c v1 v2 v3 ->
                c
                    [ encoder m1 v1
                      encoder m2 v2
                      encoder m3 v3
                    ]
            )
            (Decode.map3 ctor
                (Decode.index 0u <| decoder m1)
                (Decode.index 1u <| decoder m2)
                (Decode.index 2u <| decoder m3)
            )

    /// Define a variant with 4 parameters for a custom type.
    let variant4 name ctor m1 m2 m3 m4 : CustomCodec<_,'v> -> CustomCodec<'partial,'v> =
        variant name
            (fun c v1 v2 v3 v4 ->
                c
                    [ encoder m1 v1
                      encoder m2 v2
                      encoder m3 v3
                      encoder m4 v4
                    ]
            )
            (Decode.map4 ctor
                (Decode.index 0u <| decoder m1)
                (Decode.index 1u <| decoder m2)
                (Decode.index 2u <| decoder m3)
                (Decode.index 3u <| decoder m4)
            )

    /// Define a variant with 5 parameters for a custom type.
    let variant5 name ctor m1 m2 m3 m4 m5 : CustomCodec<_,'v> -> CustomCodec<'partial,'v> =
        variant name
            (fun c v1 v2 v3 v4 v5 ->
                c
                    [ encoder m1 v1
                      encoder m2 v2
                      encoder m3 v3
                      encoder m4 v4
                      encoder m5 v5
                    ]
            )
            (Decode.map5 ctor
                (Decode.index 0u <| decoder m1)
                (Decode.index 1u <| decoder m2)
                (Decode.index 2u <| decoder m3)
                (Decode.index 3u <| decoder m4)
                (Decode.index 4u <| decoder m5)
            )

    /// Define a variant with 6 parameters for a custom type.
    let variant6 name ctor m1 m2 m3 m4 m5 m6 : CustomCodec<_,'v> -> CustomCodec<'partial,'v> =
        variant name
            (fun c v1 v2 v3 v4 v5 v6 ->
                c
                    [ encoder m1 v1
                      encoder m2 v2
                      encoder m3 v3
                      encoder m4 v4
                      encoder m5 v5
                      encoder m6 v6
                    ]
            )
            (Decode.map6 ctor
                (Decode.index 0u <| decoder m1)
                (Decode.index 1u <| decoder m2)
                (Decode.index 2u <| decoder m3)
                (Decode.index 3u <| decoder m4)
                (Decode.index 4u <| decoder m5)
                (Decode.index 5u <| decoder m6)
            )

    /// Define a variant with 7 parameters for a custom type.
    let variant7 name ctor m1 m2 m3 m4 m5 m6 m7 : CustomCodec<_,'v> -> CustomCodec<'partial,'v> =
        variant name
            (fun c v1 v2 v3 v4 v5 v6 v7 ->
                c
                    [ encoder m1 v1
                      encoder m2 v2
                      encoder m3 v3
                      encoder m4 v4
                      encoder m5 v5
                      encoder m6 v6
                      encoder m7 v7
                    ]
            )
            (Decode.map7 ctor
                (Decode.index 0u <| decoder m1)
                (Decode.index 1u <| decoder m2)
                (Decode.index 2u <| decoder m3)
                (Decode.index 3u <| decoder m4)
                (Decode.index 4u <| decoder m5)
                (Decode.index 5u <| decoder m6)
                (Decode.index 6u <| decoder m7)
            )

    /// Define a variant with 64 parameters for a custom type.
    let variant8 name ctor m1 m2 m3 m4 m5 m6 m7 m64 =
        variant name
            (fun c v1 v2 v3 v4 v5 v6 v7 v64 ->
                c
                    [ encoder m1 v1
                      encoder m2 v2
                      encoder m3 v3
                      encoder m4 v4
                      encoder m5 v5
                      encoder m6 v6
                      encoder m7 v7
                      encoder m64 v64
                    ]
            )
            (Decode.map8 ctor
                (Decode.index 0u <| decoder m1)
                (Decode.index 1u <| decoder m2)
                (Decode.index 2u <| decoder m3)
                (Decode.index 3u <| decoder m4)
                (Decode.index 4u <| decoder m5)
                (Decode.index 5u <| decoder m6)
                (Decode.index 6u <| decoder m7)
                (Decode.index 7u <| decoder m64)
            )

    /// Build a `Codec` for a fully specified custom type.
    let buildCustom (CustomCodec am) =
        Codec
            { encoder = fun v -> am.encodeValue v
              decoder =
                Decode.field "tag" Decode.string
                    |> Decode.andThen
                        (fun tag ->
                            match Map.tryFind tag am.decoder with
                            | None ->
                                let error = "tag " + tag + "did not match"
                                Decode.fail error

                            | Some dec ->
                                Decode.field "args" dec
                        )
            }




    (* DATA STRUCTURES *)


    let composite enc dec (Codec codec) =
        Codec { encoder = enc codec.encoder
                decoder = dec codec.decoder
              }

    /// Represents an optional value.
    let option codec =
        composite Encode.option Decode.option codec

    /// `Codec` between a JSON array and a `List`.
    let list codec =
        composite
            listEncode
            Decode.list
            codec

    /// `Codec` between a JSON array and an `Array`.
    let array codec =
        composite
            (fun encoder ->
                Array.map encoder
                >> Encode.array)

            Decode.array codec

    /// `Codec` between a JSON object and a `Map`.
    let stringMap codec =
        composite
            (fun e -> Encode.stringMap << Map.map (fun _ -> e))
            Decode.stringMap
            codec

    /// `Codec` between a JSON array and a `Set`.
    let set<'a when 'a : comparison> : Codec<'a> -> Codec<Set<'a>> =
        composite
            (fun e -> listEncode e << Set.toList)
            (Decode.list >> Decode.map Set.ofList)


    /// `Codec` between a JSON array of length 2 and a `Tuple`.
    let tuple m1 m2 =
        Codec
            { encoder =
                fun (v1, v2) ->
                    listEncode id
                        [ encoder m1 v1
                          encoder m2 v2
                        ]

              decoder =
                Decode.map2
                    (fun a b -> (a, b))
                    (Decode.index 0u <| decoder m1)
                    (Decode.index 1u <| decoder m2)
            }

    /// `Codec` between a JSON array of length 3 and an triple.
    let triple m1 m2 m3 =
        Codec
            { encoder =
                fun (v1, v2, v3) ->
                    listEncode id
                        [ encoder m1 v1
                          encoder m2 v2
                          encoder m3 v3
                        ]
              decoder =
                Decode.map3
                    (fun a b c -> (a, b, c))
                    (Decode.index 0u <| decoder m1)
                    (Decode.index 1u <| decoder m2)
                    (Decode.index 2u <| decoder m3)
            }

    let result (errorCodec: Codec<'error>) (valueCodec: Codec<'value>) =
        custom
            (fun ferr fok v ->
                match v with
                | Error err ->
                    ferr err

                | Ok ok ->
                    fok ok
            )
            |> variant1 "Err" Error errorCodec
            |> variant1 "Ok" Ok valueCodec
            |> buildCustom


    (* OBJECTS *)


    /// Start creating a `Codec` for an object. You should pass the main constructor as argument.
    /// If you don't have one (for example it's a simple type with no name), you should pass a function
    /// that given the field values builds an object.
    let object ctor : ObjectCodec<'a, 'b> =
        ObjectCodec
            { encoder = fun _ -> []
              decoder = Decode.succeed ctor
            }

    /// Specify the name getter and `Codec` for a field.
    let field name (getter: 'a -> 'f) codec (ObjectCodec ocodec) : ObjectCodec<'a, 'b> =
        ObjectCodec
            { encoder = fun v -> ( name, encoder codec <| getter v ) :: ocodec.encoder v
              decoder = Decode.map2 (fun f x -> f x) ocodec.decoder (Decode.field name (decoder codec))
            }

    /// Specify the name getter and `Codec` for an optional field.
    /// This is particularly useful for evolving your `Codec`s.
    let optionalField name (getter: 'a -> 'f option) codec (ObjectCodec ocodec) : ObjectCodec<'a, 'b> =
        ObjectCodec
            { encoder = fun v -> ( name, encoder (option codec) <| getter v ) :: ocodec.encoder v
              decoder =
                decoder codec
                    |> Decode.field name
                    |> Decode.option
                    |> Decode.map2 (fun f x -> f x) ocodec.decoder
            }


    /// Create a `Codec` from a fully specified `ObjectCodec`.
    let buildObject (ObjectCodec om) =
        Codec
            { encoder = fun v -> Encode.object <| om.encoder v
              decoder = om.decoder
            }





    (* INCONSISTENT STRUCTURE *)

    /// Try a set of decoders (in order).
    /// The first argument is used for encoding and decoding, the list of other codecs is used as a fallback while decoding.
    /// This is particularly useful for backwards compatibility. You would pass the current codec as the first argument,
    /// and the old ones (eventually `map`ped) as a fallback list to use while decoding.
    let oneOf main alts =
        Codec
            { encoder = encoder main
              decoder = Decode.oneOf <| decoder main :: List.map decoder alts
            }

    /// Transform a `Codec`.
    let map go back codec =
        Codec
            { decoder = Decode.map go <| decoder codec
              encoder = fun v -> back v |> encoder codec
            }



    (* FANCY *)

    /// Ignore the JSON and make the decoder fail. This is handy when used with
    /// `oneOf` or `andThen` where you want to give a custom error message in some
    /// case. The encoder will produce `null`.
    let fail msg =
        Codec
            { decoder = Decode.fail msg
              encoder = fun _ -> Encode.nil()
            }

    /// Create codecs that depend on previous results.
    let andThen enc dec c =
        Codec
            { decoder = decoder c |> Decode.andThen (dec >> decoder)
              encoder = encoder c << enc
            }

    let private decodeLazy thunk =
        Decode.andThen thunk (Decode.succeed ())

    /// This is useful for recursive structures that are not easily modeled with `recursive`.
    /// Have a look at the Json.Decode docs for examples.
    let lazy' f =
        Codec
            { decoder = decodeLazy (fun _ -> decoder <| f ())
              encoder = fun v -> encoder (f ()) v
            }

    /// Create a `Codec` for a recursive data structure.
    /// The argument to the function you need to pass is the fully formed `Codec`.
    let rec recursive (f:Codec<'a> -> Codec<'a>) : Codec<'a> =
        f <| lazy' (fun _ -> recursive f)

    /// Create a `Codec` that produces null as JSON and always decodes as the same value.
    let constant ``default`` =
        Codec
            { decoder = Decode.succeed ``default``
              encoder = fun _ -> Encode.nil()
            }

    /// Create a `Codec` that doesn't transform the JSON value.
    let value =
        Codec
            { encoder = id
              decoder = Decode.value
            }