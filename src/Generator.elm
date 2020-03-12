module Generator exposing (Grammar(..), ProdPart(..), Production(..), Rule(..), RuleError(..), addProduction, addRule, empty, generator, parseRule, prodParser)

import Debug
import Dict exposing (Dict)
import Parser exposing (..)
import Regex


type Grammar
    = Grammar (Dict String (List String))


empty =
    Grammar Dict.empty


generator : Grammar -> String
generator c =
    ""


addProduction : String -> Maybe (List String) -> Maybe (List String)
addProduction new old =
    case old of
        Nothing ->
            Just [ new ]

        Just rules ->
            Just <| new :: rules


addRule : Grammar -> String -> String -> Grammar
addRule gram prod rule =
    case gram of
        Grammar rules ->
            Grammar <| Dict.update prod (addProduction rule) rules


type ProdPart
    = Symbol String
    | Token String


type Production
    = Terminal (List ProdPart)
    | NonTerminal (List ProdPart)


type Rule
    = Rule String (List Production)


type RuleError
    = Error


parseRule : String -> String -> Rule
parseRule sym prod =
    let
        newProd =
            prodParser prod
    in
    case newProd of
        Ok parsed ->
            Rule sym [ parsed ]

        Err _ ->
            Rule sym []


prodParser : String -> Result RuleError Production
prodParser prod =
    let
        prodParts : Result RuleError (List ProdPart)
        prodParts =
            splitProdParts prod
    in
    case prodParts of
        Ok parsed ->
            if List.all symbol parsed then
                Ok (NonTerminal (List.reverse parsed))

            else
                Ok (Terminal (List.reverse parsed))

        Err x ->
            Err x


symbol : ProdPart -> Bool
symbol prodPart =
    case prodPart of
        Symbol _ ->
            True

        Token _ ->
            False


splitProdParts : String -> Result RuleError (List ProdPart)
splitProdParts prod =
    -- List.foldl makeParts (Ok []) (Regex.split (Maybe.withDefault Regex.never (Regex.fromString "#(\\S+?)#")) prod)
    List.foldl makeParts (Ok []) (String.split "#" prod)


makeParts : String -> Result RuleError (List ProdPart) -> Result RuleError (List ProdPart)
makeParts strPart done =
    case done of
        Ok parsed ->
            case modBy 2 (List.length parsed) of
                0 ->
                    Result.map (\l -> Token strPart :: l) done

                1 ->
                    Result.map (\l -> Symbol strPart :: l) done

                _ ->
                    Err Error

        Err x ->
            Err x



-- prodP : Parser (List ProdPart)
-- prodP =
--     loop [] prodParser
-- prodParser : List ProdPart -> Parser (Step (List ProdPart) (List ProdPart))
-- prodParser revPps =
--     oneOf
--         [ succeed (\pp -> Loop (Token pp :: revPps)) |= getChompedString (succeed () |. chompWhile (\c -> c /= '#'))
--         , succeed (\pp -> Loop (pp :: revPps)) |= prodSymbol
--         , succeed (Done (List.reverse revPps)) |. end
--         ]
-- prodSymbol : Parser ProdPart
-- prodSymbol =
--     succeed Symbol
--         |= (getChompedString <|
--                 succeed ()
--                     |. symbol "#"
--                     |. chompUntil "#"
--            )
