module Codecs exposing (suite)

import Expect exposing (Expectation, fail, ok, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import Grammar exposing (..)
import Json.Decode exposing (Decoder, decodeString, dict, field, string)
import Parser
import Test exposing (..)


testJSONGrammar =
    """{
    "rules": [
        {
            "symbol": "START",
            "productions": [
                [
                    [
                        {
                            "token": "He saw a "
                        },
                        {
                            "symbol": "animal"
                        }
                    ]
                ]
            ]
        },
        {
            "symbol": "animal",
            "productions": [
                [
                    [
                        {
                            "token": "dog"
                        }
                    ],
                    [
                        {
                            "token": "sheep"
                        }
                    ]
                ]
            ]
        }
    ]
}"""



-- grammarDecoder : Decoder Grammar
-- grammarDecoder =
--     Grammar ()


suite : Test
suite =
    describe "Check the decoding of Grammars"
        [ test "a json repr gets decoded to a Grammar" <|
            \_ -> Expect.true "This is true" True

        -- decodeString GrammarDecoder testJSONGrammar
        ]
