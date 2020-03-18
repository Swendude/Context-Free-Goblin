module Grammar exposing (Grammar(..), ProdPart(..), Production, RuleError(..), addProduction, addRule, empty, parseProduction, partPrint, prodPrint, productionHelper, productionParser, symbol, symbolParser, tokenParser)

import Dict exposing (Dict)
import Parser exposing (..)


type ProdPart
    = Symbol String
    | Token String


type alias Production =
    List ProdPart


type Grammar
    = Grammar (Dict String (List Production))


empty : Grammar
empty =
    Grammar Dict.empty


partPrint : ProdPart -> String
partPrint pp =
    case pp of
        Symbol p ->
            "#" ++ p ++ "#"

        Token p ->
            p


prodPrint : Production -> String
prodPrint p =
    String.join " " <| List.map partPrint p


addRule : Grammar -> String -> String -> Result (List DeadEnd) Grammar
addRule gram sym rProduction =
    let
        newProduction =
            parseProduction rProduction
    in
    case gram of
        Grammar rules ->
            case newProduction of
                Ok prod ->
                    Ok <| Grammar <| Dict.update sym (addProduction prod) rules

                Err error ->
                    Err error


type RuleError
    = Error


addProduction : Production -> Maybe (List Production) -> Maybe (List Production)
addProduction new oldl =
    case oldl of
        Nothing ->
            Just [ new ]

        Just l ->
            Just (new :: l)


parseProduction : String -> Result (List DeadEnd) Production
parseProduction prod =
    Parser.run productionParser prod


symbol : ProdPart -> Bool
symbol prodPart =
    case prodPart of
        Symbol _ ->
            True

        Token _ ->
            False


productionParser : Parser (List ProdPart)
productionParser =
    succeed identity
        |= loop [] productionHelper


productionHelper : List ProdPart -> Parser (Step (List ProdPart) (List ProdPart))
productionHelper parts =
    oneOf
        [ symbolParser |> map (\part -> Loop (part :: parts))
        , tokenParser |> map (\part -> Loop (part :: parts))
        , end |> (\_ -> succeed (Done (List.reverse parts)))
        ]


tokenParser : Parser ProdPart
tokenParser =
    chompUntilEndOr "#"
        |> getChompedString
        |> andThen
            (\part ->
                if String.isEmpty part then
                    problem "Expected at least one character"

                else
                    succeed (Token part)
            )


symbolParser : Parser ProdPart
symbolParser =
    Parser.token "#"
        |. chompUntil "#"
        |. Parser.token "#"
        |> getChompedString
        |> andThen
            (\part ->
                let
                    strippedPart =
                        part |> String.dropLeft 1 |> String.dropRight 1
                in
                if String.isEmpty strippedPart then
                    problem "Expected at least one character"

                else
                    succeed (Symbol strippedPart)
            )
