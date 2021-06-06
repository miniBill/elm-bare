module Codec.Bare exposing
    ( Codec, Encoder, Bytes
    , Decoder, decoder, decodeValue
    , encoder, encodeToValue
    , uint
    , int
    , u8, u16, u32, u64
    , i8, i16, i32
    , f32, f64
    , bool
    , enum, enumWithValues
    , string, char
    , dataWithLength, data
    , void
    , optional
    , listWithLength
    , list
    , arrayWithLength
    , array
    , set
    , dict
    , TaggedUnionCodec, taggedUnion, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildTaggedUnion
    , StructCodec, struct, field, buildStruct
    , map, andThen
    , lazy, recursive
    )

{-| This module is an implementation of [the BARE format](https://baremessages.org/),
which at the time of writing is an [IETF draft](https://tools.ietf.org/html/draft-devault-bare-00).

The names of the functions correspond, as far as possible, to the ones in the original specification.

Most of the following documentation is taken from
[the original specification (CC-BY-SA)](https://git.sr.ht/~sircmpwn/bare),
and while the code is licensed with a MIT license, this documentation is thus CC-BY-SA itself.

Binary Application Record Encoding (BARE) is, as the name implies, a simple
binary representation for structured application data.

BARE messages omit type information, and are not self-describing. The structure
of a message must be established out of band, generally by prior agreement and
context - for example, if a BARE message is returned from /api/user/info, it
can be inferred from context that the message represents user information, and
the structure of such messages is available in the documentation for this API.


# Types

@docs Codec, Encoder, Bytes


# Decode

@docs Decoder, decoder, decodeValue


# Encode

@docs encoder, encodeToValue


# Primitive Types


## Integers

@docs uint
@docs int


## Unsigned fixed precision integers

Unsigned integers of a fixed precision, respectively 8, 16, 32, and 64 bits.
They are encoded in little-endian (least significant octet first).

@docs u8, u16, u32, u64


## Signed fixed precision integers

Signed integers of a fixed precision, respectively 8, 16, and 32 bits. `elm-bare` does not support signed 64 bits integers yet (PRs welcome).
They are encoded in little-endian (least significant octet first), with two's compliment notation.

@docs i8, i16, i32


## Floating point numbers

Floating-point numbers represented with the IEEE 754 binary32 and binary64 floating point number formats.

@docs f32, f64


## Others

@docs bool
@docs enum, enumWithValues
@docs string, char
@docs dataWithLength, data
@docs void


# Aggregate types

@docs optional
@docs listWithLength
@docs list
@docs arrayWithLength
@docs array
@docs set
@docs dict


# Tagged unions

@docs TaggedUnionCodec, taggedUnion, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildTaggedUnion


# Structs

@docs StructCodec, struct, field, buildStruct


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

A `Codec a` contains a `Bytes.Decoder a` and the corresponding `a -> Bytes.Encoder`.

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
decoder (Codec p) =
    p.decoder


{-| Run a `Codec` to turn a sequence of bytes into an Elm value.
-}
decodeValue : Codec a -> Bytes -> Maybe a
decodeValue (Codec p) =
    BD.decode p.decoder



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
encoder : Codec a -> a -> Encoder
encoder (Codec p) =
    p.encoder


{-| Convert an Elm value into a sequence of bytes.
-}
encodeToValue : Codec a -> a -> Bytes
encodeToValue (Codec p) =
    BE.encode << p.encoder



--Built-in Types


build : (a -> Encoder) -> Decoder a -> Codec a
build e d =
    Codec
        { encoder = e
        , decoder = d
        }


{-| An unsigned integer with a variable-length encoding. Each octet of the encoded value has the most-significant bit set, except for the last octet. The remaining bits are the integer value in 7-bit groups, least-significant first.

The maximum precision of such a number is 56-bits (64-bits in the original spec, but Elm `Int`s start to become unreliable after 56-bits, so caveat emptor). The maximum length of an encoded uint should thus be 8 octects but can be up to 10 octects for out-of-range integers.

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


{-| A signed integer with a variable-length encoding. Signed integers are represented as uint using a "zig-zag" encoding: positive values x are written as 2x + 0, negative values are written as 2(^x) + 1. In other words, negative numbers are complemented and whether to complement is encoded in bit 0

The maximum precision of such a number is 56-bits (64-bits in the original spec, but Elm `Int`s start to become unreliable after 56-bits, so caveat emptor). The maximum length of an encoded uint should thus be 8 octects but can be up to 10 octects for out-of-range integers.

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


{-| -}
u8 : Codec Int
u8 =
    build BE.unsignedInt8 BD.unsignedInt8


{-| -}
u16 : Codec Int
u16 =
    build (BE.unsignedInt16 Bytes.LE) (BD.unsignedInt16 Bytes.LE)


{-| -}
u32 : Codec Int
u32 =
    build (BE.unsignedInt32 Bytes.LE) (BD.unsignedInt32 Bytes.LE)


{-| WARNING (from the official Elm docs): Note: `Int` math is well-defined in the range -2^31 to 2^31 - 1.
Outside of that range, the behavior is determined by the compilation target.
When generating JavaScript, the safe range expands to -2^53 to 2^53 - 1 for some operations,
but if we generate WebAssembly some day, we would do the traditional integer overflow.
This quirk is necessary to get good performance on quirky compilation targets.
-}
u64 : Codec Int
u64 =
    map
        (\( l, h ) -> l + h * (2 ^ 32))
        (\n -> ( modBy (2 ^ 32) n, n // (2 ^ 32) ))
        (tuple u32 u32)


{-| -}
i8 : Codec Int
i8 =
    build BE.signedInt8 BD.signedInt8


{-| -}
i16 : Codec Int
i16 =
    build (BE.signedInt16 Bytes.LE) (BD.signedInt16 Bytes.LE)


{-| -}
i32 : Codec Int
i32 =
    build (BE.signedInt32 Bytes.LE) (BD.signedInt32 Bytes.LE)


{-| Due to Elm`Float\`s being 64-bit, encoding and decoding it as a 32-bit float won't be exactly equal to the original value.
-}
f32 : Codec Float
f32 =
    build (BE.float32 Bytes.LE) (BD.float32 Bytes.LE)


{-| -}
f64 : Codec Float
f64 =
    build (BE.float64 Bytes.LE) (BD.float64 Bytes.LE)


{-| A boolean value, either true or false, encoded as a u8 type with a value of one or zero, respectively representing true or false.

If a value other than one or zero is found in the u8 representation of the bool, the message is considered invalid.

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


{-| An unsigned integer value from a set of possible values agreed upon in advance, encoded with the uint type.

An enum whose uint value is not a member of the values agreed upon in advance is considered invalid.

Note that this makes the enum type unsuitable for representing several enum values which have been combined with a bitwise OR operation.

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
            |> BD.andThen
                (\i ->
                    case getAt i values of
                        Just r ->
                            BD.succeed r

                        Nothing ->
                            BD.fail
                )
        )


{-| An unsigned integer value from a set of possible values agreed upon in advance, encoded with the uint type.

This version allows you to specify how values will be encoded.

An enum whose uint value is not a member of the values agreed upon in advance is considered invalid.

Note that this makes the enum type unsuitable for representing several enum values which have been combined with a bitwise OR operation.

-}
enumWithValues : List ( a, Int ) -> Codec a
enumWithValues values =
    build
        (\value ->
            case List.filter (Tuple.first >> (==) value) values of
                [] ->
                    uintEncoder (List.length values)

                ( _, v ) :: _ ->
                    uintEncoder v
        )
        (uintDecoder
            |> BD.andThen
                (\i ->
                    case List.filter (Tuple.second >> (==) i) values of
                        ( v, _ ) :: _ ->
                            BD.succeed v

                        [] ->
                            BD.fail
                )
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


{-| A string of text. The length of the text in octets is encoded first as a uint, followed by the text data represented with the UTF-8 encoding.

If the data is found to contain invalid UTF-8 sequences, it is considered invalid.

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


{-| A single `Char`. It is encoded as a string.
-}
char : Codec Char
char =
    build
        (String.fromChar >> encoder string)
        (decoder string
            |> BD.andThen
                (String.toList >> List.head >> Maybe.map BD.succeed >> Maybe.withDefault BD.fail)
        )


{-| Arbitrary data with a fixed "length" in octets, e.g. `dataWithLength 16`. The data is encoded literally in the message.
-}
dataWithLength : Int -> Codec Bytes
dataWithLength length =
    Codec
        { encoder = BE.bytes
        , decoder = BD.bytes length
        }


{-| Arbitrary data of a variable length in octets. The length is encoded first as a uint, followed by the data itself encoded literally.
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


{-| A type with zero length. It is not encoded into BARE messages.
-}
void : Codec ()
void =
    build
        (\_ -> BE.sequence [])
        (BD.succeed ())



-- Aggregate types


{-| A value which may or may not be present. Represented as either a u8 with a value of zero, indicating that the optional value is unset; or a u8 with a value of one, followed by the encoded data of the optional type.

An optional value whose initial u8 is set to a number other than zero or one is considered invalid.

-}
optional : Codec a -> Codec (Maybe a)
optional codec =
    build
        (\value ->
            case value of
                Just v ->
                    BE.sequence [ BE.unsignedInt8 1, encoder codec v ]

                Nothing ->
                    BE.sequence [ BE.unsignedInt8 0 ]
        )
        (BD.unsignedInt8
            |> BD.andThen
                (\i ->
                    case i of
                        0 ->
                            BD.succeed Nothing

                        1 ->
                            BD.map Just <| decoder codec

                        _ ->
                            BD.fail
                )
        )


{-| A list of values of an uniform type, with a fixed length. The length is not encoded into the message. The encoded values of each member of the list are concatenated to form the encoded list.
-}
listWithLength : Int -> Codec a -> Codec (List a)
listWithLength length codec =
    build
        (BE.sequence << List.map (encoder codec))
        (BD.loop ( length, [] ) (listStep (decoder codec)) |> BD.map List.reverse)


{-| A variable-length list of values of an uniform type. The length of the list (in values) is encoded as a uint, followed by the encoded values of each member of the list concatenated.
-}
list : Codec a -> Codec (List a)
list codec =
    Codec
        { encoder = \ls -> BE.sequence <| (uintEncoder <| List.length ls) :: List.map (encoder codec) ls
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


{-| An array of values of an uniform type, with a fixed length. The length is not encoded into the message. The encoded values of each member of the array are concatenated to form the encoded array.
-}
arrayWithLength : Int -> Codec a -> Codec (Array a)
arrayWithLength length codec =
    listWithLength length codec |> map Array.fromList Array.toList


{-| A variable-length array of values of an uniform type. The length of the array (in values) is encoded as a uint, followed by the encoded values of each member of the array concatenated.
-}
array : Codec a -> Codec (Array a)
array codec =
    list codec |> map Array.fromList Array.toList


{-| A variable-length set of values of an uniform type. The length of the set (in values) is encoded as a uint, followed by the encoded values of each member of the set concatenated.
-}
set : Codec comparable -> Codec (Set comparable)
set codec =
    list codec |> map Set.fromList Set.toList


{-| An associative list of values of type `a` keyed by values of type `comparable`, e.g. `dict u32 string`. The encoded representation of a map begins with the number of key/value pairs as a uint, followed by the encoded key/value pairs concatenated. Each key/value pair is encoded as the encoded key concatenated with the encoded value.

This is not called `map` to avoid clashing with the common usage of the term in Elm.

-}
dict : Codec comparable -> Codec a -> Codec (Dict comparable a)
dict keyCodec valueCodec =
    list (tuple keyCodec valueCodec) |> map Dict.fromList Dict.toList


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
                Tuple.pair
                (decoder m1)
                (decoder m2)
        }



-- TAGGED UNION


{-| A partially built `Codec` for a tagged union (called a custom type in Elm).
-}
type TaggedUnionCodec match v
    = TaggedUnionCodec
        { match : match
        , decoder : Int -> Decoder v -> Decoder v
        , idCodec : Codec Int
        }


{-| Starts building a `Codec` for a tagged union.

A tagged union is a value that can be one of any type from a set. Each type in the set is assigned a numeric representation, starting at zero and incrementing for each type. The value is encoded as the selected tag as a uint, followed by the value itself encoded as that type.

`elm-bare` allows you to map this directly to Elm custom types. For easier interoperability with other languages you should restrict yourself to `variant0` and `variant1` (using `struct` to simulate multiple arguments).

You need to pass a pattern matching function, see the FAQ for details.

    type Semaphore
        = Red Int String Bool
        | Yellow Float
        | Green

    semaphoreCodec : Codec Semaphore
    semaphoreCodec =
        Codec.taggedUnion
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
            |> Codec.buildTaggedUnion

-}
taggedUnion : match -> TaggedUnionCodec match value
taggedUnion match =
    TaggedUnionCodec
        { match = match
        , decoder = \_ -> identity
        , idCodec = uint
        }


variant :
    Int
    -> ((List Encoder -> Encoder) -> a)
    -> Decoder v
    -> TaggedUnionCodec (a -> b) v
    -> TaggedUnionCodec b v
variant name matchPiece decoderPiece (TaggedUnionCodec am) =
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
    TaggedUnionCodec
        { match = am.match <| matchPiece enc
        , decoder = decoder_
        , idCodec = am.idCodec
        }


{-| Define a variant with 0 parameters for a tagged union.
-}
variant0 :
    Int
    -> v
    -> TaggedUnionCodec (Encoder -> a) v
    -> TaggedUnionCodec a v
variant0 name ctor =
    variant name
        (\c -> c [])
        (BD.succeed ctor)


{-| Define a variant with 1 parameters for a tagged union.
-}
variant1 :
    Int
    -> (a -> v)
    -> Codec a
    -> TaggedUnionCodec ((a -> Encoder) -> b) v
    -> TaggedUnionCodec b v
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


{-| Define a variant with 2 parameters for a tagged union.
-}
variant2 :
    Int
    -> (a -> b -> v)
    -> Codec a
    -> Codec b
    -> TaggedUnionCodec ((a -> b -> Encoder) -> c) v
    -> TaggedUnionCodec c v
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


{-| Define a variant with 3 parameters for a tagged union.
-}
variant3 :
    Int
    -> (a -> b -> c -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> TaggedUnionCodec ((a -> b -> c -> Encoder) -> partial) v
    -> TaggedUnionCodec partial v
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


{-| Define a variant with 4 parameters for a tagged union.
-}
variant4 :
    Int
    -> (a -> b -> c -> d -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> TaggedUnionCodec ((a -> b -> c -> d -> Encoder) -> partial) v
    -> TaggedUnionCodec partial v
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


{-| Define a variant with 5 parameters for a tagged union.
-}
variant5 :
    Int
    -> (a -> b -> c -> d -> e -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> TaggedUnionCodec ((a -> b -> c -> d -> e -> Encoder) -> partial) v
    -> TaggedUnionCodec partial v
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


{-| Define a variant with 6 parameters for a tagged union.
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
    -> TaggedUnionCodec ((a -> b -> c -> d -> e -> f -> Encoder) -> partial) v
    -> TaggedUnionCodec partial v
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


{-| Define a variant with 7 parameters for a tagged union.
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
    -> TaggedUnionCodec ((a -> b -> c -> d -> e -> f -> g -> Encoder) -> partial) v
    -> TaggedUnionCodec partial v
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


{-| Define a variant with 8 parameters for a tagged union.
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
    -> TaggedUnionCodec ((a -> b -> c -> d -> e -> f -> g -> h -> Encoder) -> partial) v
    -> TaggedUnionCodec partial v
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


{-| Build a `Codec` for a fully specified tagged union.
-}
buildTaggedUnion : TaggedUnionCodec (a -> Encoder) a -> Codec a
buildTaggedUnion (TaggedUnionCodec am) =
    Codec
        { encoder = \v -> am.match v
        , decoder =
            decoder am.idCodec
                |> BD.andThen
                    (\tag ->
                        am.decoder tag BD.fail
                    )
        }



-- STRUCTS


{-| A partially built `Codec` for a struct (called an object in Elm).
-}
type StructCodec a b
    = StructCodec
        { encoder : a -> List Encoder
        , decoder : Decoder b
        }


{-| Start creating a `Codec` for a struct. You should pass the main constructor as argument.

A struct is a set of values of arbitrary types, concatenated together in an order known in advance.

If you don't have one (for example it's a simple type with no name), you should pass a function that given the field values builds an object.

    type alias Point =
        { x : Int
        , y : Int
        }

    pointCodec : Codec Point
    pointCodec =
        Codec.struct Point
            |> Codec.field .x Codec.signedInt
            |> Codec.field .y Codec.signedInt
            |> Codec.buildStruct

-}
struct : b -> StructCodec a b
struct ctor =
    StructCodec
        { encoder = \_ -> []
        , decoder = BD.succeed ctor
        }


{-| Specify how to get a value from the struct we want to encode and then give a `Codec` for that value.
-}
field : (a -> f) -> Codec f -> StructCodec a (f -> b) -> StructCodec a b
field getter codec (StructCodec ocodec) =
    StructCodec
        { encoder = \v -> (encoder codec <| getter v) :: ocodec.encoder v
        , decoder = BD.map2 (\f x -> f x) ocodec.decoder (decoder codec)
        }


{-| Create a `Codec` from a fully specified `StructCodec`.
-}
buildStruct : StructCodec a a -> Codec a
buildStruct (StructCodec om) =
    Codec
        { encoder = om.encoder >> List.reverse >> BE.sequence
        , decoder = om.decoder
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
