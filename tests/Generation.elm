module Generation exposing (suite)

import ExampleGrammars exposing (..)
import Expect exposing (ok, true)
import Expect.Extra
import Grammar exposing (Grammar(..), generateSentence, pickRule)
import Test exposing (Test, describe, test)


seed : Int
seed =
    42


suite : Test
suite =
    describe "Check the generation of sentences"
        [ describe "A grammar produces valid sentences"
            [ test "a random generated sentence is in the list of valid sentences (1-deep, non-recursive)" <|
                \_ ->
                    Expect.true "expect sentence to be in valid list (1-deep, non-recursive)" <| List.member (generateSentence seed (Grammar normalGrammarRules)) normalGrammarSentences
            , test "a generated sentence is equal to the deterministic result" <|
                \_ ->
                    Expect.equal (Ok "He saw a dog in a forest") (generateSentence seed (Grammar deterministicGrammarRules))
            , test "a random generated sentence is in the list of valid sentences (n-deep, non-recursive)" <|
                \_ ->
                    Expect.Extra.member (Result.withDefault "") (generateSentence seed (Grammar deepGrammarRules)) deepGrammarSentences
            ]
        , describe "Rule picking works"
            [ test "Picking a rule works" <|
                \_ ->
                    Expect.ok <| pickRule seed (Grammar normalGrammarRules) "START"
            , test "Picking a rule results in a valid rule" <|
                \_ ->
                    Expect.true "picking from animal results in an animal choice" <| List.member (Result.withDefault [] <| pickRule seed (Grammar normalGrammarRules) "animal") possibleAnimals
            ]
        , describe "Recursive grammars do not crash"
            [ test "a random generated sentence from an invalid recursive grammar does not crash (1-deep, recursive)" <|
                \_ ->
                    Expect.err <| generateSentence seed (Grammar recursiveInvalidGrammarRules)
            ]
        ]
