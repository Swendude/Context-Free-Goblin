module Codecs exposing (suite)

import Debug
import Dict exposing (Dict, fromList)
import ExampleGrammars as EG
import Expect exposing (Expectation, fail, ok, pass)
import Fuzz
import Grammar exposing (..)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Parser
import Test exposing (..)


grammarDecoder : Decoder Grammar
grammarDecoder =
    JD.map Grammar <| JD.field "rules" rulesDecoder


rulesDecoder : Decoder (Dict String (List Production))
rulesDecoder =
    JD.map Dict.fromList <| ruleDecoder


ruleDecoder : Decoder (List ( String, List Production ))
ruleDecoder =
    JD.list <| JD.map2 Tuple.pair (JD.field "symbol" JD.string) (JD.field "productions" prodsDecoder)


prodsDecoder : Decoder (List Production)
prodsDecoder =
    JD.list prodDecoder


prodDecoder : Decoder (List ProdPart)
prodDecoder =
    JD.field "type" JD.string
        |> JD.andThen prodPartFromType
        |> JD.list


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
    JD.map Token (JD.field "str" JD.string)


symbolDecoder : Decoder ProdPart
symbolDecoder =
    JD.map Symbol (JD.field "str" JD.string)


jsonGrammarRules =
    Dict.fromList
        [ ( "START", [ [ Token "He saw a ", Symbol "animal" ] ] )
        , ( "animal", [ [ Token "dog" ], [ Token "sheep" ] ] )
        ]


grammarEncoder : Grammar -> Value
grammarEncoder (Grammar rules) =
    JE.object
        [ ( "rules", JE.list rulesEncoder (Dict.toList rules) )
        ]


rulesEncoder : ( String, List Production ) -> Value
rulesEncoder ( sym, prods ) =
    JE.object
        [ ( "symbol", JE.string sym )
        , ( "productions", JE.list prodEncoder prods )
        ]


prodEncoder : Production -> Value
prodEncoder prod =
    JE.list prodPartEncoder prod


prodPartEncoder : ProdPart -> Value
prodPartEncoder prodPart =
    case prodPart of
        Symbol str ->
            JE.object
                [ ( "type"
                  , JE.string "symbol"
                  )
                , ( "str"
                  , JE.string str
                  )
                ]

        Token str ->
            JE.object
                [ ( "type"
                  , JE.string "token"
                  )
                , ( "str"
                  , JE.string str
                  )
                ]


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
