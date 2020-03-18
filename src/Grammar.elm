module Grammar exposing (GeneratorError(..), Grammar(..), ProdPart(..), Production, RuleError(..), addProduction, addRule, empty, generateSentence, isSymbol, parseProduction, pickRule, prodPartToString, prodToString, productionHelper, productionParser, replaceSymbols, resolveProdPart, symbolParser, tokenParser)

import Dict exposing (Dict)
import Parser exposing (..)
import Random exposing (initialSeed, step)
import Random.List exposing (shuffle)
import Result.Extra exposing (combine)
import Test exposing (..)


type ProdPart
    = Symbol String
    | Token String


type alias Production =
    List ProdPart


type Grammar
    = Grammar (Dict String (List Production))



-- Construction


empty : Grammar
empty =
    Grammar Dict.empty



-- Printing


prodPartToString : ProdPart -> String
prodPartToString pp =
    case pp of
        Symbol s ->
            "#" ++ s ++ "#"

        Token s ->
            s


prodToString : Production -> String
prodToString prod =
    List.map prodPartToString prod |> String.join ""



-- UTILITY


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



-- PARSER


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


isSymbol : ProdPart -> Bool
isSymbol prodPart =
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



-- Generator


type GeneratorError
    = NoStartRule
    | SymbolMissing String
    | EmptyProduction
    | RecursionError


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


replaceSymbols : Int -> Grammar -> Production -> Result GeneratorError Production
replaceSymbols c gram cur =
    let
        replaced =
            List.map (resolveProdPart gram) cur |> combine |> Result.map List.concat
    in
    case replaced of
        Ok newprod ->
            if c > 100 then
                Err RecursionError

            else if List.any isSymbol newprod then
                replaceSymbols (c + 1) gram newprod

            else
                Ok newprod

        Err err ->
            Err err


generateSentence : Grammar -> Result GeneratorError String
generateSentence (Grammar rules) =
    if Dict.member "START" rules then
        let
            first =
                pickRule (Grammar rules) "START"
        in
        case first of
            Ok prod ->
                replaceSymbols 0 (Grammar rules) prod |> Result.map prodToString

            Err err ->
                Err err

    else
        Err NoStartRule
