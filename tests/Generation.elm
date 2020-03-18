module Generation exposing (suite)

import Expect exposing (Expectation, fail, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import Generator exposing (..)
import Parser
import Test exposing (..)
import Dict

testGrammar : Grammar
testGrammar = Grammar <| Dict.fromList [("Start", [Generator.NonTerminal [Token "He saw a ", Symbol "animal", Token "in a ", Symbol "location"]])
                                    ,("animal", [Generator.Terminal [Token "dog"], Generator.Terminal [Token "sheep"]])
                                    ,("location", [Generator.Terminal [Token "forest"], Generator.Terminal [Token "city"]])]

testGrammarSentences : List String
testGrammarSentences = ["He saw a dog in a forest", "He saw a sheep in a forest", "He saw a dog in a city", "He saw a sheep in a forest"]

suite : Test
suite =
    describe "Check the generation of sentences"
        [ describe "A terminal production comes back as Terminal type"
            [ test "a production without symbols is a terminal" <|
                \_ ->
                    let
                        res =
                            parseProduction "He saw a dog"
                    in
                    case res of
                        Ok prods ->
                            Expect.equal (Terminal [ Token "He saw a dog" ]) prods

                        Err _ ->
                            fail "Parsing went wrong"
            ]

        ]
