module Grammar exposing (GeneratorError(..), Grammar(..), ProdPart(..), Production, RuleError(..), addProduction, addRule, empty, generateSentence, getSymbols, isSymbol, parseProduction, pickRule, prodPartToString, prodToString, productionHelper, productionParser, replaceSymbols, resolveProdPart, symbolParser, tokenParser)

import Dict exposing (Dict)
import Parser exposing (..)
import Random exposing (initialSeed, step)
import Random.List exposing (shuffle)
import Result.Extra exposing (combine)


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


getSymbols : Grammar -> List String
getSymbols (Grammar rules) =
    Dict.keys rules



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


pickRule : Random.Seed -> Grammar -> String -> ( Random.Seed, Result GeneratorError Production )
pickRule seed (Grammar rules) symbol =
    case Dict.get symbol rules of
        Just productions ->
            let
                seedAndChoice =
                    step (shuffle productions) seed

                choice =
                    List.head <| Tuple.first <| seedAndChoice

                newseed =
                    Tuple.second seedAndChoice
            in
            case choice of
                Just prod ->
                    ( newseed, Ok prod )

                Nothing ->
                    ( newseed, Err EmptyProduction )

        Nothing ->
            ( seed, Err (SymbolMissing symbol) )


resolveProdPart : Grammar -> ProdPart -> ( Random.Seed, List (Result GeneratorError Production) ) -> ( Random.Seed, List (Result GeneratorError Production) )
resolveProdPart gram prodPart ( seed, curProd ) =
    case prodPart of
        Token str ->
            ( seed, List.append [ Ok <| [ Token str ] ] curProd )

        Symbol sym ->
            let
                ( new_seed, choice ) =
                    pickRule seed gram sym
            in
            ( new_seed, List.append [ choice ] curProd )


replaceSymbols : Random.Seed -> Int -> Grammar -> Production -> Result GeneratorError Production
replaceSymbols seed recursionCounter gram cur =
    let
        replaced =
            List.foldr (resolveProdPart gram) ( seed, [] ) cur |> Tuple.second |> combine |> Result.map List.concat
    in
    case replaced of
        Ok newprod ->
            if recursionCounter > 100 then
                Err RecursionError

            else if List.any isSymbol newprod then
                replaceSymbols seed (recursionCounter + 1) gram newprod

            else
                Ok newprod

        Err err ->
            Err err


generateSentence : Random.Seed -> Grammar -> Result GeneratorError String
generateSentence seed (Grammar rules) =
    if Dict.member "START" rules then
        let
            choice =
                Tuple.second <| pickRule seed (Grammar rules) "START"
        in
        case choice of
            Ok prod ->
                replaceSymbols seed 0 (Grammar rules) prod |> Result.map prodToString

            Err err ->
                Err err

    else
        Err NoStartRule
