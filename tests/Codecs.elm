module Codecs exposing (suite)

import Codec exposing (grammarDecoder, grammarEncoder)
import ExampleGrammars as EG
import Expect exposing (ok)
import Grammar exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (..)


suite : Test
suite =
    describe "Test codec form grammars"
        [ describe "Check the decoding of Grammars"
            [ test "a json repr gets decoded to a Grammar" <|
                \_ ->
                    Expect.ok <|
                        JD.decodeString grammarDecoder EG.testJSONGrammar
            , test "a json repr gets decoded to the right Grammar" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString grammarDecoder EG.testJSONGrammar)
                        (Ok <| Grammar EG.jsonGrammarRules)
            ]
        , describe "check the encoding of Grammars"
            [ test "a Grammar gets encoded to json repr" <|
                \_ ->
                    Expect.equal
                        EG.testJSONGrammar
                        (JE.encode 4 (grammarEncoder (Grammar EG.jsonGrammarRules)))
            ]
        , describe "check matching of total codec"
            [ test "encoding and then decoding results in the same Grammar" <|
                \_ ->
                    Expect.equal
                        (JD.decodeString grammarDecoder (JE.encode 4 (grammarEncoder (Grammar EG.jsonGrammarRules))))
                        (Ok (Grammar EG.jsonGrammarRules))
            ]
        ]
