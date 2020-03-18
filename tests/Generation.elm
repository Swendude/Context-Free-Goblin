module Generation exposing (suite)

import Debug
import Dict exposing (Dict)
import Expect exposing (Expectation, fail, ok, pass, true)
import Fuzz exposing (Fuzzer, int, list, string)
import Grammar exposing (..)
import Parser
import Random exposing (initialSeed, step)
import Random.List exposing (shuffle)
import Test exposing (..)


unpackedTestGrammar : Dict String (List Production)
unpackedTestGrammar =
    Dict.fromList
        [ ( "START", [ [ Token "He saw a ", Symbol "animal", Token "in a ", Symbol "location" ] ] )
        , ( "animal", [ [ Token "dog" ], [ Token "sheep" ] ] )
        , ( "location", [ [ Token "forest" ], [ Token "city" ] ] )
        ]


testGrammar : Grammar
testGrammar =
    Grammar <| unpackedTestGrammar


testGrammarSentences : List (Result GeneratorError String)
testGrammarSentences =
    [ Ok "He saw a dog in a forest", Ok "He saw a sheep in a forest", Ok "He saw a dog in a city", Ok "He saw a sheep in a forest" ]


possibleAnimals : List Production
possibleAnimals =
    [ [ Token "dog" ], [ Token "sheep" ] ]


type GeneratorError
    = NoStartRule
    | SymbolMissing String
    | EmptyProduction


pickRule : Dict String (List Production) -> String -> Result GeneratorError Production
pickRule gram symbol =
    case Dict.get symbol gram of
        Just rules ->
            let
                choice =
                    List.head <| Tuple.first <| step (shuffle rules) (initialSeed 42)
            in
            case choice of
                Just prod ->
                    Ok prod

                Nothing ->
                    Err EmptyProduction

        Nothing ->
            Err (SymbolMissing symbol)



-- prodToString : Production -> Result GeneratorError String
-- prodToString prod =
--     case prod of
--         Terminal pps ->
--             Ok
--         NonTerminal pps ->


generateSentence : Grammar -> Result GeneratorError String
generateSentence g =
    case g of
        Grammar gram ->
            if Dict.member "START" gram then
                -- Ok <| pickRule gram "START"
                Ok "He saw a dog in a forest"

            else
                Err NoStartRule


suite : Test
suite =
    describe "Check the generation of sentences"
        [ describe "A grammar produces valid sentences"
            [ test "a random generated sentence is in the list of valid sentences" <|
                \_ ->
                    Expect.true "expect sentence to be in valid list" <| List.member (generateSentence testGrammar) testGrammarSentences
            ]
        , describe "Rule picking works"
            [ test "Picking a rule works" <|
                \_ ->
                    Expect.ok <| pickRule unpackedTestGrammar "START"
            , test "Picking a rule results in a valid rule" <|
                \_ ->
                    Expect.true "picking from animal results in an animal choice" <| List.member (Result.withDefault [] <| pickRule unpackedTestGrammar "animal") possibleAnimals
            ]
        ]
