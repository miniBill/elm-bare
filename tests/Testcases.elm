module Testcases exposing (suite)

import Bytes
import Bytes.Decode
import Bytes.Encode
import Codec.Bare as Codec exposing (Bytes, Codec)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Testing spec compliance"
        [ describe "Basic" basicTests
        ]


basicTests : List Test
basicTests =
    [ roundtrip "u8" Codec.u8 0x42 [ 0x42 ]
    , roundtrip "u16" Codec.u16 0xCAFE [ 0xFE, 0xCA ]
    , roundtrip "u32" Codec.u32 0xDEADBEEF [ 0xEF, 0xBE, 0xAD, 0xDE ]

    --, roundtrip "u64" Codec.u64 0xCAFEBABEDEADBEEF [ 0xEF, 0xBE, 0xAD, 0xDE, 0xBE, 0xBA, 0xFE, 0xCA ]
    , roundtrip "u" Codec.uint 0xDEADBEEF [ 0xEF, 0xFD, 0xB6, 0xF5, 0x0D ]
    , roundtrip "i8 (-)" Codec.i8 -42 [ 0xD6 ]
    , roundtrip "i8 (+)" Codec.i8 42 [ 42 ]
    , roundtrip "i16" Codec.i16 -1234 [ 0x2E, 0xFB ]
    , roundtrip "i32" Codec.i32 -12345678 [ 0xB2, 0x9E, 0x43, 0xFF ]

    --, roundtrip "i64" Codec.i64 -12345678987654321 [ 0x4F, 0x0B, 0x6E, 0x9D, 0xAB, 0x23, 0xD4, 0xFF ]
    , roundtrip "i" Codec.int -12345678 [ 0x9B, 0x85, 0xE3, 0x0B ]
    , roundtripWithin 0.0001 "f32" Codec.f32 1337.42 [ 0x71, 0x2D, 0xA7, 0x44 ]
    , roundtrip "f64" Codec.f64 133713371337.42424 [ 0x9B, 0x6C, 0xC9, 0x20, 0xF0, 0x21, 0x3F, 0x42 ]
    , roundtrip "b (T)" Codec.bool True [ 0x01 ]
    , roundtrip "b (F)" Codec.bool False [ 0x00 ]
    , roundtrip "str" Codec.string "こんにちは、世界！" [ 0x1B, 0xE3, 0x81, 0x93, 0xE3, 0x82, 0x93, 0xE3, 0x81, 0xAB, 0xE3, 0x81, 0xA1, 0xE3, 0x81, 0xAF, 0xE3, 0x80, 0x81, 0xE4, 0xB8, 0x96, 0xE7, 0x95, 0x8C, 0xEF, 0xBC, 0x81 ]
    ]


roundtripWithin : number -> String -> Codec number -> number -> List Int -> Test
roundtripWithin tolerance desc codec value raw =
    let
        forth =
            Codec.encodeToValue codec value
                |> bytesToList

        back =
            raw
                |> listToBytes
                |> Codec.decodeValue codec
                |> Maybe.withDefault -9999999
    in
    describe (desc ++ " roundtrips")
        [ test "value -> bytes" <|
            \_ ->
                Expect.equal raw forth
        , test "bytes -> value" <|
            \_ ->
                Expect.all
                    [ Expect.atLeast (value - tolerance)
                    , Expect.atMost (value + tolerance)
                    ]
                    back
        ]


roundtrip : String -> Codec b -> b -> List Int -> Test
roundtrip desc codec value raw =
    let
        forth =
            Codec.encodeToValue codec value
                |> bytesToList

        back =
            raw
                |> listToBytes
                |> Codec.decodeValue codec
    in
    describe (desc ++ " roundtrips")
        [ test "value -> bytes" <|
            \_ ->
                Expect.equal raw forth
        , test "bytes -> value" <|
            \_ ->
                Expect.equal (Just value) back
        ]


listToBytes : List Int -> Bytes
listToBytes list =
    Bytes.Encode.encode <| Bytes.Encode.sequence <| List.map Bytes.Encode.unsignedInt8 list


bytesToList : Bytes.Bytes -> List Int
bytesToList bytes =
    Bytes.Decode.loop ( Bytes.width bytes, [] ) bytesListStep
        |> Bytes.Decode.map List.reverse
        |> (\dec -> Bytes.Decode.decode dec bytes)
        |> Maybe.withDefault []


bytesListStep : ( Int, List Int ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List Int ) (List Int))
bytesListStep ( n, xs ) =
    if n <= 0 then
        Bytes.Decode.succeed (Bytes.Decode.Done xs)

    else
        Bytes.Decode.map (\x -> Bytes.Decode.Loop ( n - 1, x :: xs )) Bytes.Decode.unsignedInt8
