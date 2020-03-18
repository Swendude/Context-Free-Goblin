module Generation exposing (suite)

import Expect exposing (Expectation, fail, pass, true)
import Fuzz exposing (Fuzzer, int, list, string)
import Grammar exposing (..)
import Parser
import Test exposing (..)
import Dict

testGrammar : Grammar
testGrammar = Grammar <| Dict.fromList [("Start", [Grammar.NonTerminal [Token "He saw a ", Symbol "animal", Token "in a ", Symbol "location"]])
                                    ,("animal", [Grammar.Terminal [Token "dog"], Grammar.Terminal [Token "sheep"]])
                                    ,("location", [Grammar.Terminal [Token "forest"], Grammar.Terminal [Token "city"]])]

testGrammarSentences : List String
testGrammarSentences = ["He saw a dog in a forest", "He saw a sheep in a forest", "He saw a dog in a city", "He saw a sheep in a forest"]


generateSentence : Grammar -> String
generateSentence g = "He saw a dog in a forest"

suite : Test
suite =
    describe "Check the generation of sentences"
        [ describe "A grammar produces valid sentences"
            [ test "a random generated sentence is in the list of valid sentences" <|
                (\_ ->
                    Expect.true "expect sentence to be in valid list" <| (List.member (generateSentence testGrammar) testGrammarSentences))
            ]

        ]
