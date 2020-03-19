module Codecs exposing (suite)

import Debug
import Dict exposing (Dict, fromList)
import Expect exposing (Expectation, fail, ok, pass)
import Fuzz
import Grammar exposing (..)
import Json.Decode as JD exposing (Decoder, decodeString, dict, field, list, map, map2, oneOf, string)
import Parser
import Test exposing (..)


testJSONGrammar =
    """{
    "rules": [
        {
            "symbol": "START",
            "productions": [
                [
                    {
                        "type": "token",
                        "str": "He saw a "
                    },
                    {
                        "type": "symbol",
                        "str": "animal"
                    }
                ]
            ]
        },
        {
            "symbol": "animal",
            "productions": [
                [
                    {
                        "type": "token",
                        "str": "dog"
                    }
                ],
                [
                    {
                        "type": "token",
                        "str": "sheep"
                    }
                ]
            ]
        }
    ]
}"""


grammarDecoder : Decoder Grammar
grammarDecoder =
    map Grammar <| field "rules" rulesDecoder


rulesDecoder : Decoder (Dict String (List Production))
rulesDecoder =
    map Dict.fromList <| ruleDecoder


ruleDecoder : Decoder (List ( String, List Production ))
ruleDecoder =
    list <| map2 Tuple.pair (field "symbol" string) (field "productions" prodsDecoder)


prodsDecoder : Decoder (List Production)
prodsDecoder =
    list prodDecoder


prodDecoder : Decoder (List ProdPart)
prodDecoder =
    field "type" string
        |> JD.andThen prodPartFromType
        |> list


prodPartFromType : String -> Decoder ProdPart
prodPartFromType str =
    case str of
        "symbol" ->
            symbolDecoder

        "token" ->
            tokenDecoder

        _ ->
            JD.fail ("Invalid type: " ++ str)


tokenDecoder : Decoder ProdPart
tokenDecoder =
    JD.map Token (field "str" string)


symbolDecoder : Decoder ProdPart
symbolDecoder =
    JD.map Symbol (field "str" string)


suite : Test
suite =
    describe "Check the decoding of Grammars"
        [ test "a json repr gets decoded to a Grammar" <|
            \_ ->
                Expect.ok <|
                    decodeString grammarDecoder testJSONGrammar
        ]
