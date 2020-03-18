module Generation exposing (suite)

import Debug
import Dict exposing (Dict)
import Expect exposing (Expectation, fail, ok, pass, true)
import Fuzz exposing (Fuzzer, int, list, string)
import Grammar exposing (..)
import Parser
import Random exposing (initialSeed, step)
import Random.List exposing (shuffle)
import Result.Extra exposing (combine)
import Test exposing (..)


normalGrammarRules : Dict String (List Production)
normalGrammarRules =
    Dict.fromList
        [ ( "START", [ [ Token "He saw a ", Symbol "animal", Token " in a ", Symbol "location" ] ] )
        , ( "animal", [ [ Token "dog" ], [ Token "sheep" ] ] )
        , ( "location", [ [ Token "forest" ], [ Token "city" ] ] )
        ]


deterministicGrammarRules : Dict String (List Production)
deterministicGrammarRules =
    Dict.fromList
        [ ( "START", [ [ Token "He saw a ", Symbol "animal", Token " in a ", Symbol "location" ] ] )
        , ( "animal", [ [ Token "dog" ] ] )
        , ( "location", [ [ Token "forest" ] ] )
        ]


normalGrammar : Grammar
normalGrammar =
    Grammar <| normalGrammarRules


deterministicGrammar : Grammar
deterministicGrammar =
    Grammar <| deterministicGrammarRules


normalGrammarSentences : List (Result GeneratorError String)
normalGrammarSentences =
    [ Ok "He saw a dog in a forest", Ok "He saw a sheep in a forest", Ok "He saw a dog in a city", Ok "He saw a sheep in a forest" ]


possibleAnimals : List Production
possibleAnimals =
    [ [ Token "dog" ], [ Token "sheep" ] ]


type GeneratorError
    = NoStartRule
    | SymbolMissing String
    | EmptyProduction


pickRule : Grammar -> String -> Result GeneratorError Production
pickRule (Grammar rules) symbol =
    case Dict.get symbol rules of
        Just productions ->
            let
                choice =
                    List.head <| Tuple.first <| step (shuffle productions) (initialSeed 42)
            in
            case choice of
                Just prod ->
                    Ok prod

                Nothing ->
                    Err EmptyProduction

        Nothing ->
            Err (SymbolMissing symbol)


resolveProdPart : Grammar -> ProdPart -> Result GeneratorError Production
resolveProdPart gram prodPart =
    case prodPart of
        Token str ->
            Ok <| [ Token str ]

        Symbol sym ->
            pickRule gram sym


replaceSymbols : Grammar -> Production -> Result GeneratorError Production
replaceSymbols gram cur =
    List.map (resolveProdPart gram) cur |> combine |> Result.map List.concat


generateSentence : Grammar -> Result GeneratorError String
generateSentence (Grammar rules) =
    if Dict.member "START" rules then
        let
            first =
                pickRule (Grammar rules) "START"
        in
        case first of
            Ok prod ->
                replaceSymbols (Grammar rules) prod |> Result.map prodToString

            Err err ->
                Err err

    else
        Err NoStartRule


suite : Test
suite =
    describe "Check the generation of sentences"
        [ describe "A grammar produces valid sentences"
            [ test "a random generated sentence is in the list of valid sentences" <|
                \_ ->
                    Expect.true "expect sentence to be in valid list" <| List.member (generateSentence normalGrammar) normalGrammarSentences
            , test "a generated sentence is equal to the deterministic result" <|
                \_ ->
                    Expect.equal (Ok "He saw a dog in a forest") (generateSentence deterministicGrammar)
            ]
        , describe "Rule picking works"
            [ test "Picking a rule works" <|
                \_ ->
                    Expect.ok <| pickRule normalGrammar "START"
            , test "Picking a rule results in a valid rule" <|
                \_ ->
                    Expect.true "picking from animal results in an animal choice" <| List.member (Result.withDefault [] <| pickRule normalGrammar "animal") possibleAnimals
            ]
        ]
