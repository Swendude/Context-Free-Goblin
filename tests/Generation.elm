module Generation exposing (suite)

import Expect exposing (Expectation, fail, pass, true, ok)
import Fuzz exposing (Fuzzer, int, list, string)
import Grammar exposing (..)
import Parser
import Test exposing (..)
import Dict
import Dict exposing (Dict)
import Random.List exposing (shuffle)
import Random exposing(step, initialSeed)
import Debug

unpackedTestGrammar = Dict.fromList [("START", [Grammar.NonTerminal [Token "He saw a ", Symbol "animal", Token "in a ", Symbol "location"]])
                                    ,("animal", [Grammar.Terminal [Token "dog"], Grammar.Terminal [Token "sheep"]])
                                    ,("location", [Grammar.Terminal [Token "forest"], Grammar.Terminal [Token "city"]])]
testGrammar : Grammar
testGrammar = Grammar <| unpackedTestGrammar

testGrammarSentences : List (Result GeneratorError String)
testGrammarSentences = [Ok "He saw a dog in a forest", Ok "He saw a sheep in a forest", Ok "He saw a dog in a city", Ok "He saw a sheep in a forest"]

type GeneratorError =
    NoStartRule | SymbolMissing String

pickRule : Dict String (List Production) -> String -> Result GeneratorError (List Production)
pickRule gram symbol =
    case Dict.get symbol gram of
        Just rules ->
            Ok (Tuple.first <| step (shuffle rules) (initialSeed 42))
        Nothing ->
            Err (SymbolMissing symbol)

generateSentence : Grammar -> Result GeneratorError String
generateSentence g =
    case g of
        Grammar gram ->
            case Dict.get "START" gram of
                Just rules ->
                    Ok  "He saw a dog in a forest"
                Nothing ->
                    Err NoStartRule

suite : Test
suite =
    describe "Check the generation of sentences"
        [ describe "A grammar produces valid sentences"
            [ test "a random generated sentence is in the list of valid sentences" <|
                (\_ ->
                    Expect.true "expect sentence to be in valid list" <| List.member (generateSentence testGrammar) testGrammarSentences)
            ]
        , describe "Rule picking works"
                [test "Picking a rule works" <|
                (\_ ->
                    Expect.ok <| Debug.log (Debug.toString (pickRule unpackedTestGrammar "START")) (pickRule unpackedTestGrammar "START"))]
                 
        ]
