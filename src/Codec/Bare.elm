module Codec.Bare exposing
    ( Codec, Encoder, Bytes
    , Decoder, decoder, decodeValue
    , encoder, encodeToValue
    , uint, int
    , u8, u16, u32, u64
    , i8, i16, i32, i64
    , f32, f64
    , enum
    , string
    , fixedLengthData, bytes, data
    , void
    , maybe
    , fixedLengthList, list
    , fixedLengthArray, array
    , fixedLengthSet, set
    , dict
    , CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom
    , ObjectCodec, object, field, buildObject
    , map, andThen
    , lazy, recursive
    , buildStruct, buildTaggedUnion, char, constant, customWithIdCodec, float32, float64, optional, result, signedInt, signedInt16, signedInt32, signedInt8, stringbool, struct, taggedUnion, triple, tuple, unsignedInt, unsignedInt16, unsignedInt32, unsignedInt8
    )

{-| A `Codec a` contains a `Bytes.Decoder a` and the corresponding `a -> Bytes.Encoder`.

This module is an implementation of the BARE format.
The types are fully compatible with `Codec.Bytes`, but if you want to ensure interoperability with other BARE implementation
you should use only functions from this module.

The names of the functions correspond, as far as possible, to the ones in the original specification.
For some functions synonyms more familiar for Elm users are provided.

Most of the following documentation is taken from
[the original specification (CC-BY-SA)](https://git.sr.ht/~sircmpwn/bare),
and while the code is licensed with a MIT license, this documentation is CC-BY-SA itself.

Binary Application Record Encoding (BARE) is, as the name implies, a simple
binary representation for structured application data.

BARE messages omit type information, and are not self-describing. The structure
of a message must be established out of band, generally by prior agreement and
context - for example, if a BARE message is returned from /api/user/info, it
can be inferred from context that the message represents user information, and
the structure of such messages is available in the documentation for this API.


# Definition

@docs Codec, Encoder, Bytes


# Decode

@docs Decoder, decoder, decodeValue


# Encode

@docs encoder, encodeToValue


# Built-in Types

@docs uint, int
@docs u8, u16, u32, u64
@docs i8, i16, i32, i64
@docs f32, f64
@docs bool
@docs enum
@docs string
@docs fixedLengthData, bytes, data
@docs void


# Aggregate types

@docs maybe
@docs fixedLengthList, list
@docs fixedLengthArray, array
@docs fixedLengthSet, set
@docs dict
@docs CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom
@docs ObjectCodec, object, field, buildObject


# Mapping

@docs map, andThen


# Fancy Codecs

@docs lazy, recursive

-}

import Array exposing (Array)
import Bitwise
import Bytes
import Bytes.Decode as BD
import Bytes.Encode as BE
import Dict exposing (Dict)
import Set exposing (Set)



-- DEFINITION


{-| A value that knows how to encode and decode a sequence of bytes.
-}
type Codec a
    = Codec
        { encoder : a -> Encoder
        , decoder : Decoder a
        }


{-| Describes how to generate a sequence of bytes.
-}
type alias Encoder =
    BE.Encoder


{-| A sequence of bytes. Refer to the [elm/bytes docs][bytes] for more information.

[bytes]: https://package.elm-lang.org/packages/elm/bytes/latest/Bytes#Bytes

-}
type alias Bytes =
    Bytes.Bytes



-- DECODE


{-| Describes how to turn a sequence of bytes into a nice Elm value.
-}
type alias Decoder a =
    BD.Decoder a


{-| Extracts the `Decoder` contained inside the `Codec`.
-}
decoder : Codec a -> Decoder a
decoder =
    CB.decoder


{-| Run a `Codec` to turn a sequence of bytes into an Elm value.
-}
decodeValue : Codec a -> Bytes -> Maybe a
decodeValue =
    CB.decodeValue



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
encoder : Codec a -> a -> Encoder
encoder =
    CB.encoder


{-| Convert an Elm value into a sequence of bytes.
-}
encodeToValue : Codec a -> a -> Bytes
encodeToValue =
    CB.encodeToValue



--Built-in Types


{-| `Codec` between an unsigned variable-length integer and an Elm `Int`.

A variable-length integer. Each octet of the encoded value has
the most-significant bit set, except for the last octet. The
remaining bits are the integer value in 7-bit groups,
least-significant first.

The maximum precision of a varint is 56 bit (64 bit in the original spec,
but Elm `Int`s start to become unreliable after 56 bits).

-}
uint : Codec Int
uint =
    build uintEncoder uintDecoder


uintEncoder : Int -> Encoder
uintEncoder =
    let
        toList i =
            if i < 128 then
                [ i ]

            else
                (128 + modBy 128 i) :: (toList <| i // 128)
    in
    BE.sequence << List.map BE.unsignedInt8 << toList


uintDecoder : Decoder Int
uintDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\h ->
                if h < 128 then
                    BD.succeed h

                else
                    BD.map (\ti -> (h - 128) + 128 * ti) uintDecoder
            )


{-| `Codec` between a signed variable-length integer and an Elm `Int`.

A variable-length integer. Each octet of the encoded value has
the most-significant bit set, except for the last octet. The
remaining bits are the integer value in 7-bit groups,
least-significant first.

Signed integers are mapped to unsigned integers using "zig-zag"
encoding: positive values x are written as `2 * x + 0`, negative
values are written as `2 * (~x) + 1`; that is, negative numbers are
complemented and whether to complement is encoded in bit 0.

The maximum precision of a varint is 56 bit (64 bit in the original spec,
but Elm `Int`s start to become unreliable after 56 bits).

-}
int : Codec Int
int =
    map
        (\u ->
            if modBy 2 u == 1 then
                Bitwise.complement <| u // 2

            else
                u // 2
        )
        (\s ->
            if s < 0 then
                Bitwise.complement s * 2 + 1

            else
                s * 2
        )
        uint


{-| `Codec` between an unsigned 8-bit integer and an Elm `Int`
-}
u8 : Codec Int
u8 =
    CB.unsignedInt8


{-| `Codec` between an unsigned 16-bit integer and an Elm `Int`
-}
u16 : Codec Int
u16 =
    build (BE.unsignedInt16 Bytes.LE) (BD.unsignedInt16 Bytes.LE)


{-| `Codec` between an unsigned 32-bit integer and an Elm `Int`
-}
u32 : Codec Int
u32 =
    build (BE.unsignedInt32 Bytes.LE) (BD.unsignedInt32 Bytes.LE)


{-| `Codec` between a signed 8-bit integer and an Elm `Int`
-}
i8 : Codec Int
i8 =
    CB.signedInt8


{-| `Codec` between a signed 16-bit integer and an Elm `Int`
-}
i16 : Codec Int
i16 =
    build (BE.signedInt16 Bytes.LE) (BD.signedInt16 Bytes.LE)


{-| `Codec` between a signed 32-bit integer and an Elm `Int`
-}
i32 : Codec Int
i32 =
    build (BE.signedInt32 Bytes.LE) (BD.signedInt32 Bytes.LE)


{-| `Codec` between a 32-bit float and an Elm `Float`.
Due to Elm `Float`s being 64-bit, encoding and decoding it as a 32-bit float won't exactly equal the original value.
-}
f32 : Codec Float
f32 =
    build (BE.float32 Bytes.LE) (BD.float32 Bytes.LE)


{-| `Codec` between a 64-bit float and an Elm `Float`
-}
f64 : Codec Float
f64 =
    build (BE.float64 Bytes.LE) (BD.float64 Bytes.LE)


{-| `Codec` between a sequence of bytes and an Elm `Bool`
-}
bool : Codec Bool
bool =
    build
        (\value ->
            if value then
                BE.unsignedInt8 1

            else
                BE.unsignedInt8 0
        )
        (BD.unsignedInt8
            |> BD.andThen
                (\value ->
                    case value of
                        0 ->
                            BD.succeed False

                        _ ->
                            BD.succeed True
                )
        )


{-| A value from a set of possible values enumerated in advance, encoded as a uint.
-}
enum : List a -> Codec a
enum values =
    build
        (\value ->
            findIndexHelp 0 ((==) value) values
                |> Maybe.withDefault (List.length values)
                |> uintEncoder
        )
        (uintDecoder
            |> BD.andThen (\i -> getAt i values)
        )


{-| <https://github.com/elm-community/list-extra/blob/8.2.4/src/List/Extra.elm#L630>
-}
findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate ls =
    case ls of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs


{-| <https://github.com/elm-community/list-extra/blob/8.2.4/src/List/Extra.elm#L122>
-}
getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs


{-| `Codec` between a sequence of bytes and an Elm `String`
-}
string : Codec String
string =
    build
        (\text ->
            BE.sequence
                [ uintEncoder (BE.getStringWidth text)
                , BE.string text
                ]
        )
        (uintDecoder |> BD.andThen (\charCount -> BD.string charCount))


{-| Arbitrary binary data with a fixed "length" in bytes, e.g.
`fixedLengthData 16`.
-}
fixedLengthData : Int -> Codec Bytes
fixedLengthData length =
    Codec
        { encoder = BE.bytes
        , decoder = BD.bytes length
        }


{-| Arbitrary binary data of an undefined length.
-}
data : Codec Bytes
data =
    Codec
        { encoder =
            \bytes_ ->
                BE.sequence
                    [ uintEncoder (Bytes.width bytes_)
                    , BE.bytes bytes_
                    ]
        , decoder = uintDecoder |> BD.andThen (\length -> BD.bytes length)
        }


{-| Synonym of `data`
-}
bytes : Codec Bytes
bytes =
    data


{-| A type with zero length.
-}
void : Codec {}
void =
    build
        (\_ -> BE.sequence [])
        (BD.succeed {})



-- Aggregate types


maybe : Codec a -> Codec (Maybe a)
maybe codec =
    build
        (\value ->
            case value of
                Just v ->
                    BE.sequence [ BE.unsignedInt8 1, CB.encoder codec v ]

                Nothing ->
                    BE.sequence [ BE.unsignedInt8 0 ]
        )
        (BD.unsignedInt8
            |> BD.andThen
                (\i ->
                    if i == 0 then
                        BD.succeed Nothing

                    else
                        CB.decoder codec
                )
        )


{-| An array of values with a fixed "length", e.g. [8]string.
-}
fixedLengthList : Int -> Codec a -> Codec (List a)
fixedLengthList length codec =
    build
        (BE.sequence << List.map (CB.encoder codec))
        (BD.loop ( length, [] ) (listStep (decoder codec)) |> BD.map List.reverse)


{-| `Codec` between a sequence of bytes and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list codec =
    Codec
        { encoder = \ls -> BE.sequence <| (uintEncoder <| List.length ls) :: List.map (CB.encoder codec) ls
        , decoder =
            uintDecoder
                |> BD.andThen
                    (\length -> BD.loop ( length, [] ) (listStep (decoder codec)) |> BD.map List.reverse)
        }


listStep : BD.Decoder a -> ( Int, List a ) -> Decoder (BD.Step ( Int, List a ) (List a))
listStep decoder_ ( n, xs ) =
    if n <= 0 then
        BD.succeed (BD.Done xs)

    else
        BD.map (\x -> BD.Loop ( n - 1, x :: xs )) decoder_


{-| An array of values with a fixed "length", e.g. [8]string.
-}
fixedLengthArray : Int -> Codec a -> Codec (Array a)
fixedLengthArray length codec =
    fixedLengthList codec |> map Array.fromList Array.toList


{-| `Codec` between a sequence of bytes and an Elm `Array`.
-}
array : Codec a -> Codec (Array a)
array codec =
    list codec |> map Array.fromList Array.toList


{-| A set of values with a fixed "length", e.g. [8]string.
-}
fixedLengthSet : Int -> Codec a -> Codec (Set a)
fixedLengthSet length codec =
    fixedLengthList codec |> map Set.fromList Set.toList


{-| `Codec` between a sequence of bytes and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set comparable)
set codec =
    list codec |> map Set.fromList Set.toList


{-| A map of values of type B keyed by values of type A, e.g. map[u32]string.

The order of items is undefined.

This is not called `map` to avoid clashing with the common usage of the term in Elm.

-}
dict : Codec comparable -> Codec a -> Codec (Dict comparable a)
dict keyCodec valueCodec =
    list (tuple keyCodec valueCodec) |> map Dict.fromList Dict.toList



{-
   # Aggregate types

   @docs taggedUnion, taggedUnionMember, buildTaggedUnion
   @docs custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom
   @docs struct, structField, buildStruct
   @docs object, field, buildObject


   # Mapping

   @docs map, andThen


   # Fancy Codecs

   @docs lazy, recursive

-}


{-| `Codec` between a sequence of bytes and an Elm `Char`
-}
char : Codec Char
char =
    build
        (String.fromChar >> encoder string)
        (decoder string
            |> BD.andThen
                (String.toList >> List.head >> Maybe.map BD.succeed >> Maybe.withDefault BD.fail)
        )


{-| `Codec` between a sequence of bytes and an Elm `Tuple`.
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple m1 m2 =
    Codec
        { encoder =
            \( v1, v2 ) ->
                BE.sequence
                    [ encoder m1 v1
                    , encoder m2 v2
                    ]
        , decoder =
            BD.map2
                (\a b -> ( a, b ))
                (decoder m1)
                (decoder m2)
        }


{-| `Codec` between a sequence of bytes and an Elm triple.
-}
triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
triple m1 m2 m3 =
    Codec
        { encoder =
            \( v1, v2, v3 ) ->
                BE.sequence
                    [ encoder m1 v1
                    , encoder m2 v2
                    , encoder m3 v3
                    ]
        , decoder =
            BD.map3
                (\a b c -> ( a, b, c ))
                (decoder m1)
                (decoder m2)
                (decoder m3)
        }


{-| `Codec` for `Result` values.
-}
result : Codec error -> Codec value -> Codec (Result error value)
result errorCodec valueCodec =
    custom
        (\ferr fok value ->
            case value of
                Err err ->
                    ferr err

                Ok ok ->
                    fok ok
        )
        |> variant1 0 Err errorCodec
        |> variant1 1 Ok valueCodec
        |> buildCustom



-- OBJECTS


{-| A partially built `Codec` for an object.
-}
type ObjectCodec a b
    = ObjectCodec
        { encoder : a -> List Encoder
        , decoder : Decoder b
        }


{-| Start creating a `Codec` for an object. You should pass the main constructor as argument.
If you don't have one (for example it's a simple type with no name), you should pass a function that given the field values builds an object.

    type alias Point =
        { x : Int
        , y : Int
        }

    pointCodec : Codec Point
    pointCodec =
        Codec.object Point
            |> Codec.field .x Codec.signedInt
            |> Codec.field .y Codec.signedInt
            |> Codec.buildObject

-}
object : b -> ObjectCodec a b
object ctor =
    ObjectCodec
        { encoder = \_ -> []
        , decoder = BD.succeed ctor
        }


{-| Specify how to get a value from the object we want to encode and then give a `Codec` for that value.
-}
field : (a -> f) -> Codec f -> ObjectCodec a (f -> b) -> ObjectCodec a b
field getter codec (ObjectCodec ocodec) =
    ObjectCodec
        { encoder = \v -> (encoder codec <| getter v) :: ocodec.encoder v
        , decoder = BD.map2 (\f x -> f x) ocodec.decoder (decoder codec)
        }


{-| Create a `Codec` from a fully specified `ObjectCodec`.
-}
buildObject : ObjectCodec a a -> Codec a
buildObject (ObjectCodec om) =
    Codec
        { encoder = om.encoder >> List.reverse >> BE.sequence
        , decoder = om.decoder
        }



-- CUSTOM


{-| A partially built `Codec` for a custom type.
-}
type CustomCodec match v
    = CustomCodec
        { match : match
        , decoder : Int -> Decoder v -> Decoder v
        , idCodec : Codec Int
        }


{-| Starts building a `Codec` for a custom type.
You need to pass a pattern matching function, see the FAQ for details.

    type Semaphore
        = Red Int String Bool
        | Yellow Float
        | Green

    semaphoreCodec : Codec Semaphore
    semaphoreCodec =
        Codec.custom
            (\redEncoder yellowEncoder greenEncoder value ->
                case value of
                    Red i s b ->
                        redEncoder i s b

                    Yellow f ->
                        yellowEncoder f

                    Green ->
                        greenEncoder
            )
            |> Codec.variant3 0 Red Codec.signedInt Codec.string Codec.bool
            |> Codec.variant1 1 Yellow Codec.float64
            |> Codec.variant0 2 Green
            |> Codec.buildCustom

-}
custom : match -> CustomCodec match value
custom match =
    customWithIdCodec signedInt match


variant :
    Int
    -> ((List Encoder -> Encoder) -> a)
    -> Decoder v
    -> CustomCodec (a -> b) v
    -> CustomCodec b v
variant name matchPiece decoderPiece (CustomCodec am) =
    let
        enc v =
            encoder am.idCodec name
                :: v
                |> BE.sequence

        decoder_ tag orElse =
            if tag == name then
                decoderPiece

            else
                am.decoder tag orElse
    in
    CustomCodec
        { match = am.match <| matchPiece enc
        , decoder = decoder_
        , idCodec = am.idCodec
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 :
    Int
    -> v
    -> CustomCodec (Encoder -> a) v
    -> CustomCodec a v
variant0 name ctor =
    variant name
        (\c -> c [])
        (BD.succeed ctor)


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    Int
    -> (a -> v)
    -> Codec a
    -> CustomCodec ((a -> Encoder) -> b) v
    -> CustomCodec b v
variant1 name ctor m1 =
    variant name
        (\c v ->
            c
                [ encoder m1 v
                ]
        )
        (BD.map ctor
            (decoder m1)
        )


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    Int
    -> (a -> b -> v)
    -> Codec a
    -> Codec b
    -> CustomCodec ((a -> b -> Encoder) -> c) v
    -> CustomCodec c v
variant2 id ctor m1 m2 =
    variant id
        (\c v1 v2 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                ]
        )
        (BD.map2 ctor
            (decoder m1)
            (decoder m2)
        )


{-| Define a variant with 3 parameters for a custom type.
-}
variant3 :
    Int
    -> (a -> b -> c -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> CustomCodec ((a -> b -> c -> Encoder) -> partial) v
    -> CustomCodec partial v
variant3 id ctor m1 m2 m3 =
    variant id
        (\c v1 v2 v3 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                ]
        )
        (BD.map3 ctor
            (decoder m1)
            (decoder m2)
            (decoder m3)
        )


{-| Define a variant with 4 parameters for a custom type.
-}
variant4 :
    Int
    -> (a -> b -> c -> d -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> CustomCodec ((a -> b -> c -> d -> Encoder) -> partial) v
    -> CustomCodec partial v
variant4 id ctor m1 m2 m3 m4 =
    variant id
        (\c v1 v2 v3 v4 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                ]
        )
        (BD.map4 ctor
            (decoder m1)
            (decoder m2)
            (decoder m3)
            (decoder m4)
        )


{-| Define a variant with 5 parameters for a custom type.
-}
variant5 :
    Int
    -> (a -> b -> c -> d -> e -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> CustomCodec ((a -> b -> c -> d -> e -> Encoder) -> partial) v
    -> CustomCodec partial v
variant5 id ctor m1 m2 m3 m4 m5 =
    variant id
        (\c v1 v2 v3 v4 v5 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                , encoder m5 v5
                ]
        )
        (BD.map5 ctor
            (decoder m1)
            (decoder m2)
            (decoder m3)
            (decoder m4)
            (decoder m5)
        )


{-| Define a variant with 6 parameters for a custom type.
-}
variant6 :
    Int
    -> (a -> b -> c -> d -> e -> f -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> Encoder) -> partial) v
    -> CustomCodec partial v
variant6 id ctor m1 m2 m3 m4 m5 m6 =
    variant id
        (\c v1 v2 v3 v4 v5 v6 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                , encoder m5 v5
                , encoder m6 v6
                ]
        )
        (BD.map5 (\a b c d ( e, f ) -> ctor a b c d e f)
            (decoder m1)
            (decoder m2)
            (decoder m3)
            (decoder m4)
            (BD.map2 Tuple.pair
                (decoder m5)
                (decoder m6)
            )
        )


{-| Define a variant with 7 parameters for a custom type.
-}
variant7 :
    Int
    -> (a -> b -> c -> d -> e -> f -> g -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Codec g
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> g -> Encoder) -> partial) v
    -> CustomCodec partial v
variant7 id ctor m1 m2 m3 m4 m5 m6 m7 =
    variant id
        (\c v1 v2 v3 v4 v5 v6 v7 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                , encoder m5 v5
                , encoder m6 v6
                , encoder m7 v7
                ]
        )
        (BD.map5 (\a b c ( d, e ) ( f, g ) -> ctor a b c d e f g)
            (decoder m1)
            (decoder m2)
            (decoder m3)
            (BD.map2 Tuple.pair
                (decoder m4)
                (decoder m5)
            )
            (BD.map2 Tuple.pair
                (decoder m6)
                (decoder m7)
            )
        )


{-| Define a variant with 8 parameters for a custom type.
-}
variant8 :
    Int
    -> (a -> b -> c -> d -> e -> f -> g -> h -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Codec g
    -> Codec h
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> g -> h -> Encoder) -> partial) v
    -> CustomCodec partial v
variant8 id ctor m1 m2 m3 m4 m5 m6 m7 m8 =
    variant id
        (\c v1 v2 v3 v4 v5 v6 v7 v8 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                , encoder m5 v5
                , encoder m6 v6
                , encoder m7 v7
                , encoder m8 v8
                ]
        )
        (BD.map5 (\a b ( c, d ) ( e, f ) ( g, h ) -> ctor a b c d e f g h)
            (decoder m1)
            (decoder m2)
            (BD.map2 Tuple.pair
                (decoder m3)
                (decoder m4)
            )
            (BD.map2 Tuple.pair
                (decoder m5)
                (decoder m6)
            )
            (BD.map2 Tuple.pair
                (decoder m7)
                (decoder m8)
            )
        )


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustom : CustomCodec (a -> Encoder) a -> Codec a
buildCustom (CustomCodec am) =
    Codec
        { encoder = \v -> am.match v
        , decoder =
            decoder am.idCodec
                |> BD.andThen
                    (\tag ->
                        am.decoder tag BD.fail
                    )
        }



-- MAPPING


{-| Transform a `Codec`.
-}
map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map fromBytes toBytes codec =
    Codec
        { decoder = BD.map fromBytes <| decoder codec
        , encoder = \v -> toBytes v |> encoder codec
        }


{-| Transform a `Codec` in a way that can potentially fail when decoding.

    {-| Volume must be between 0 and 1.
    -}
    volumeCodec =
        Codec.float64
            |> Codec.andThen
                (\volume ->
                    if volume <= 1 && volume >= 0 then
                        Just volume

                    else
                        Nothing
                )
                (\volume -> volume)

Note that this function is a bit risky.
If you encode data that fails to decode, you won't get any indication as to what happened.

-}
andThen : (a -> Maybe b) -> (b -> a) -> Codec a -> Codec b
andThen fromBytes toBytes codec =
    Codec
        { decoder =
            decoder codec
                |> BD.andThen
                    (\value ->
                        case fromBytes value of
                            Just newValue ->
                                BD.succeed newValue

                            Nothing ->
                                BD.fail
                    )
        , encoder = \v -> toBytes v |> encoder codec
        }



-- FANCY


{-| Create a `Codec` for a recursive data structure.
The argument to the function you need to pass is the fully formed `Codec`, see the FAQ for details.

    type Peano
        = Peano (Maybe Peano)

    peanoCodec : Codec Peano
    peanoCodec =
        Codec.recursive
            (\finishedCodec ->
                Codec.maybe finishedCodec
                    |> Codec.map Peano (\(Peano p) -> p)
            )

-}
recursive : (Codec a -> Codec a) -> Codec a
recursive f =
    let
        step =
            { decoder = BD.succeed () |> BD.andThen (\() -> recursive f |> decoder)
            , encoder = \value -> encoder (recursive f) value
            }
    in
    f <| Codec step


{-| This is useful for recursive structures that are not easily modeled with `recursive`.

    type Peano
        = Peano (Maybe Peano)

    {-| This is the same example used in Codec.recursive but adapted for lazy.
    -}
    peanoCodec : Codec Peano
    peanoCodec =
        Codec.maybe (Codec.lazy (\() -> peanoCodec)) |> Codec.map Peano (\(Peano a) -> a)

-}
lazy : (() -> Codec a) -> Codec a
lazy f =
    Codec
        { decoder = BD.succeed () |> BD.andThen (\() -> decoder (f ()))
        , encoder = \value -> encoder (f ()) value
        }


{-| Same as `custom` but here we can choose what codec to use for the integer id we tell apart variants with.
This is useful if, for example, you know you won't have ids outside of the range 0 - 255 and can use unsignedInt8 instead of the default signedInt32 to save some space.
-}
customWithIdCodec : Codec Int -> match -> CustomCodec match value
customWithIdCodec idCodec match =
    CustomCodec
        { match = match
        , decoder = \_ -> identity
        , idCodec = idCodec
        }


{-| Create a `Codec` that encodes nothing and always decodes as the same value.
-}
constant : a -> Codec a
constant default_ =
    Codec
        { decoder = BD.succeed default_
        , encoder = \_ -> BE.sequence []
        }
