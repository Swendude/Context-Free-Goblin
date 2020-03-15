module Parsing exposing (suite)

import Expect exposing (Expectation, fail, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import Generator exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Check the parsing of productions"
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
        , describe "A nonterminal production comes back as NonTerminal type"
            [ test "a production with symbols is a nonterminal" <|
                \_ ->
                    let
                        res =
                            parseProduction "He saw a #animal#"
                    in
                    case res of
                        Ok prods ->
                            Expect.equal (NonTerminal [ Token "He saw a ", Symbol "animal", Token ""]) prods

                        Err _ ->
                            fail "Parsing went wrong"
            ]
        , describe "Using Elm/Parser works"
            [ test "strings get parsed" <|
                 \_ -> Expect.equal (parseProd "He saw a dog") (Ok (Terminal [ Token "He saw a dog" ]))
            ]
        ]
