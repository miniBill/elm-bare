# elm-bare
The goal of this package is to implement the [BARE Message Encoding](https://baremessages.org/) format in Elm, using a "`Codec`" API.

## Basic usage

Round-tripping is done through the use of encoders (`a -> Encoder`) and decoders (`Decoder a`) for a sequence of bytes, collectively called a `Codec a`.

```elm
import Codec.Bare as Codec exposing (Bytes, Codec, Encoder)

codec : Codec (List Int)
codec =
    Codec.list Codec.int

encode : List Int -> Bytes
encode list =
    Codec.encodeToValue codec list

decode : Bytes -> Maybe (List Int)
decode s =
    Codec.decodeValue codec s
```

## Learning Resources

Ask for help on the [Elm Slack](https://elmlang.herokuapp.com/).

You can also have a look at the `FAQ.md` file.

## Credits
This project is a fork of [MartinSStewart/elm-codec-bytes](https://package.elm-lang.org/packages/MartinSStewart/elm-codec-bytes/latest/) (which is itself inspired by `miniBill/elm-codec`), specialized for being compatible with the [BARE Message Encoding](https://baremessages.org/).
