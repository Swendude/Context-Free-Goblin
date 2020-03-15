module Generator exposing (Grammar(..), ProdPart(..), Production(..), RuleError(..), addProduction, addRule, empty, makeParts, parseProduction, partPrint, prodPrint, splitProdParts, symbol)

import Debug
import Dict exposing (Dict)
import Regex


type ProdPart
    = Symbol String
    | Token String


type Production
    = Terminal (List ProdPart)
    | NonTerminal (List ProdPart)


type Grammar
    = Grammar (Dict String (List Production))


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
    case p of
        Terminal pp ->
            "T: " ++ (String.join " " <| List.map partPrint pp)

        NonTerminal pp ->
            "NT: " ++ (String.join " " <| List.map partPrint pp)



-- generator : Grammar -> String
-- generator c =
--     ""


addRule : Grammar -> String -> String -> Result RuleError Grammar
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


parseProduction : String -> Result RuleError Production
parseProduction prod =
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
