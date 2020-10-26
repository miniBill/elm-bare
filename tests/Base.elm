module Base exposing (roundtrips, suite)

import Bytes
import Bytes.Encode
import Codec.Bare as Codec exposing (Bytes, Codec)
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Set
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Testing roundtrips"
        [ describe "Basic" basicTests
        , describe "Containers" containersTests
        , describe "Object" objectTests
        , describe "Custom" customTests
        , roundtrips "bimap" Fuzz.float <|
            Codec.map
                (\x -> x * 2)
                (\x -> x / 2)
                Codec.f64
        , describe "andThen" andThenTests
        , roundtrips "lazy" peanoFuzz peanoCodec

        -- This test is expected to fail. Unfortunately it's very difficult to make lazy and recursive stack safe.
        --, roundtripsConstant "lazy stackoverflow"
        --    (List.repeat 10000 () |> List.foldl (\() peano -> Peano (Just peano)) (Peano Nothing))
        --    peanoCodec
        , describe "maybe" maybeTests
        , describe "constant"
            [ test "roundtrips"
                (\_ ->
                    Codec.constant 632
                        |> (\d -> Codec.decodeValue d (Bytes.Encode.sequence [] |> Bytes.Encode.encode))
                        |> Expect.equal (Just 632)
                )
            ]
        , describe "recursive" recursiveTests
        ]


roundtrips : String -> Fuzzer a -> Codec a -> Test
roundtrips name fuzzer codec =
    fuzz fuzzer name <|
        \value ->
            value
                |> Codec.encodeToValue codec
                |> Codec.decodeValue codec
                |> Expect.equal (Just value)


roundtripsConstant : String -> a -> Codec a -> Test
roundtripsConstant name value codec =
    test name <|
        \() ->
            value
                |> Codec.encodeToValue codec
                |> Codec.decodeValue codec
                |> Expect.equal (Just value)


roundtripsWithin : String -> Fuzzer Float -> Codec Float -> Test
roundtripsWithin name fuzzer codec =
    fuzz fuzzer name <|
        \value ->
            value
                |> Codec.encodeToValue codec
                |> Codec.decodeValue codec
                |> Maybe.withDefault -999.1234567
                |> Expect.within (Expect.Relative 0.000001) value


basicTests : List Test
basicTests =
    [ roundtrips "Codec.string" Fuzz.string Codec.string
    , roundtrips "Codec.string with unicode chars" (Fuzz.constant "Ⓐ弈😀") Codec.string
    , roundtrips "Codec.int" signedInt32Fuzz Codec.int
    , roundtrips "Codec.uint" unsignedInt32Fuzz Codec.uint
    , roundtrips "Codec.f64" Fuzz.float Codec.f64
    , roundtripsWithin "Codec.f32" Fuzz.float Codec.f32
    , roundtrips "Codec.bool" Fuzz.bool Codec.bool
    , roundtrips "Codec.char" Fuzz.char Codec.char
    , roundtrips "Codec.bytes" fuzzBytes Codec.bytes
    ]


fuzzBytes : Fuzzer Bytes
fuzzBytes =
    Fuzz.list unsignedInt32Fuzz |> Fuzz.map (List.map (Bytes.Encode.unsignedInt32 Bytes.LE) >> Bytes.Encode.sequence >> Bytes.Encode.encode)


containersTests : List Test
containersTests =
    [ roundtrips "Codec.array" (Fuzz.array signedInt32Fuzz) (Codec.array Codec.int)
    , roundtrips "Codec.list" (Fuzz.list signedInt32Fuzz) (Codec.list Codec.int)
    , roundtripsConstant "Codec.list stackoverflow" (List.repeat 100000 'a') (Codec.list Codec.char)
    , roundtrips "Codec.dict"
        (Fuzz.map2 Tuple.pair Fuzz.string signedInt32Fuzz
            |> Fuzz.list
            |> Fuzz.map Dict.fromList
        )
        (Codec.dict Codec.string Codec.int)
    , roundtrips "Codec.set"
        (Fuzz.list signedInt32Fuzz |> Fuzz.map Set.fromList)
        (Codec.set Codec.int)
    , roundtrips "Codec.tuple"
        (Fuzz.tuple ( signedInt32Fuzz, signedInt32Fuzz ))
        (Codec.tuple Codec.int Codec.int)
    ]


unsignedInt32Fuzz : Fuzzer Int
unsignedInt32Fuzz =
    Fuzz.intRange 0 4294967295


signedInt32Fuzz : Fuzzer Int
signedInt32Fuzz =
    Fuzz.intRange -2147483648 2147483647


objectTests : List Test
objectTests =
    [ roundtrips "with 0 fields"
        (Fuzz.constant {})
        (Codec.object {}
            |> Codec.buildObject
        )
    , roundtrips "with 1 field"
        (Fuzz.map (\i -> { fname = i }) signedInt32Fuzz)
        (Codec.object (\i -> { fname = i })
            |> Codec.field .fname Codec.int
            |> Codec.buildObject
        )
    , roundtrips "with 2 fields"
        (Fuzz.map2
            (\a b ->
                { a = a
                , b = b
                }
            )
            signedInt32Fuzz
            signedInt32Fuzz
        )
        (Codec.object
            (\a b ->
                { a = a
                , b = b
                }
            )
            |> Codec.field .a Codec.int
            |> Codec.field .b Codec.int
            |> Codec.buildObject
        )
    ]


type Newtype a
    = Newtype a


type Newtype6 a b c d e f
    = Newtype6 a b c d e f


customTests : List Test
customTests =
    [ roundtrips "with 1 ctor, 0 args"
        (Fuzz.constant ())
        (Codec.custom
            (\f v ->
                case v of
                    () ->
                        f
            )
            |> Codec.variant0 0 ()
            |> Codec.buildCustom
        )
    , roundtrips "with 1 ctor, 1 arg"
        (Fuzz.map Newtype signedInt32Fuzz)
        (Codec.custom
            (\f v ->
                case v of
                    Newtype a ->
                        f a
            )
            |> Codec.variant1 1 Newtype Codec.int
            |> Codec.buildCustom
        )
    , roundtrips "with 1 ctor, 1 arg, different id codec"
        (Fuzz.map Newtype signedInt32Fuzz)
        (Codec.customWithIdCodec Codec.u8
            (\f v ->
                case v of
                    Newtype a ->
                        f a
            )
            |> Codec.variant1 1 Newtype Codec.int
            |> Codec.buildCustom
        )
    , roundtrips "with 1 ctor, 6 arg"
        (Fuzz.map5 (Newtype6 0) signedInt32Fuzz signedInt32Fuzz signedInt32Fuzz signedInt32Fuzz signedInt32Fuzz)
        (Codec.custom
            (\function v ->
                case v of
                    Newtype6 a b c d e f ->
                        function a b c d e f
            )
            |> Codec.variant6 1 Newtype6 Codec.int Codec.int Codec.int Codec.int Codec.int Codec.int
            |> Codec.buildCustom
        )
    , describe "misc" <|
        let
            match fnothing fjust value =
                case value of
                    Nothing ->
                        fnothing

                    Just v ->
                        fjust v

            codec =
                Codec.custom match
                    -- Use a negative id here just to make sure the default id codec handles negative values
                    |> Codec.variant0 -1 Nothing
                    |> Codec.variant1 0 Just Codec.int
                    |> Codec.buildCustom

            fuzzers =
                [ ( "1st ctor", Fuzz.constant Nothing )
                , ( "2nd ctor", Fuzz.map Just signedInt32Fuzz )
                ]
        in
        fuzzers
            |> List.map
                (\( name, fuzz ) ->
                    describe name
                        [ roundtrips "with 2 ctors, 0,1 args" fuzz codec ]
                )
    ]


volumeCodec : Codec Float
volumeCodec =
    Codec.f64
        |> Codec.andThen
            (\volume ->
                if 0 <= volume && volume <= 1 then
                    Just volume

                else
                    Nothing
            )
            (\volume -> volume)


andThenTests : List Test
andThenTests =
    [ roundtrips "roundtrips" (Fuzz.floatRange 0 1) volumeCodec
    , test "andThen fails on invalid binary data." <|
        \_ -> 5 |> Codec.encodeToValue volumeCodec |> Codec.decodeValue volumeCodec |> Expect.equal Nothing
    ]


type Peano
    = Peano (Maybe Peano)


{-| This is the same example used in Codec.recursive but adapted for lazy.
-}
peanoCodec : Codec Peano
peanoCodec =
    Codec.maybe (Codec.lazy (\() -> peanoCodec)) |> Codec.map Peano (\(Peano a) -> a)


peanoFuzz : Fuzzer Peano
peanoFuzz =
    Fuzz.intRange 0 10 |> Fuzz.map (intToPeano Nothing)


intToPeano : Maybe Peano -> Int -> Peano
intToPeano peano value =
    if value <= 0 then
        Peano Nothing

    else
        intToPeano peano (value - 1) |> Just |> Peano


maybeTests : List Test
maybeTests =
    [ roundtrips
        "single"
        (Fuzz.oneOf
            [ Fuzz.constant Nothing
            , Fuzz.map Just signedInt32Fuzz
            ]
        )
      <|
        Codec.maybe Codec.int
    ]


recursiveTests : List Test
recursiveTests =
    [ roundtrips "list" (Fuzz.list signedInt32Fuzz) <|
        Codec.recursive
            (\c ->
                Codec.custom
                    (\fempty fcons value ->
                        case value of
                            [] ->
                                fempty

                            x :: xs ->
                                fcons x xs
                    )
                    |> Codec.variant0 0 []
                    |> Codec.variant2 1 (::) Codec.int c
                    |> Codec.buildCustom
            )
    ]
