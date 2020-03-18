module Parsing exposing (suite)

import Expect exposing (Expectation, fail, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import Grammar exposing (..)
import Parser
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
                            Expect.equal [ Token "He saw a dog" ] prods

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
                            Expect.equal [ Token "He saw a ", Symbol "animal" ] prods

                        Err _ ->
                            fail "Parsing went wrong"
            ]
        , describe "productionParser works"
            [ test "productionParser parses a terminal production" <|
                \_ ->
                    Expect.equal
                        (Parser.run productionParser "He saw a dog")
                        (Ok [ Token "He saw a dog" ])
            , test "productionParser parses a nonTerminal production" <|
                \_ ->
                    Expect.equal
                        (Parser.run productionParser "#animal#")
                        (Ok [ Symbol "animal" ])
            , test "productionParser parses a production" <|
                \_ ->
                    Expect.equal
                        (Parser.run productionParser "He saw a #animal#")
                        (Ok [ Token "He saw a ", Symbol "animal" ])
            , test "productionParser parses a production with leading and trailing tokens" <|
                \_ ->
                    Expect.equal
                        (Parser.run productionParser "He saw a #animal# far away")
                        (Ok [ Token "He saw a ", Symbol "animal", Token " far away" ])
            , test "productionParser fails on a production with leading delimites" <|
                \_ ->
                    Expect.err
                        (Parser.run productionParser "He saw a #animal")
            , test "productionParser fails on a production with trailing delimiter" <|
                \_ ->
                    Expect.err
                        (Parser.run productionParser "He saw a animal#")
            , test "productionParser errors on a production with empty symbols" <|
                \_ ->
                    Expect.err
                        (Parser.run productionParser "He saw a ##")
            ]
        , describe "tokenParser works"
            [ test "tokenParser parses a terminal production" <|
                \_ ->
                    Expect.equal
                        (Parser.run tokenParser "He saw a dog")
                        (Ok (Token "He saw a dog"))
            , test "tokenParser parses up to a symbol defintion" <|
                \_ ->
                    Expect.equal
                        (Parser.run tokenParser "He saw a #animal#")
                        (Ok (Token "He saw a "))
            , test "tokenParser fails on an empty string" <|
                \_ ->
                    Expect.err
                        (Parser.run tokenParser "")
            ]
        , describe "symbolParser works"
            [ test "symbolParser parses a nonTerminal production" <|
                \_ ->
                    Expect.equal
                        (Parser.run symbolParser "#animal#")
                        (Ok (Symbol "animal"))
            , test "symbolParser fails on a terminal production" <|
                \_ ->
                    Expect.err
                        (Parser.run symbolParser "He saw a dog")
            ]
        ]
