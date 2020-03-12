module Generator exposing (..)
import Dict exposing (Dict)
import Debug
import Parser exposing (..)
type Grammar = Grammar (Dict String (List String))

empty = Grammar Dict.empty

generator : Grammar -> String
generator c = ""  


addProduction : String -> Maybe (List String)  -> Maybe (List String)
addProduction new old = 
    case old of 
        Nothing ->
            Just [new]
        Just rules ->
            Just <| (new) :: rules

addRule : Grammar -> String -> String -> Grammar
addRule gram prod rule = 
    case gram of
        Grammar rules ->
            Grammar <| Dict.update prod (addProduction rule) rules
            
type ProdPart = Symbol String | Token String

type Production = Terminal (List ProdPart) | NonTerminal (List ProdPart)

type Rule = Rule String (List Production)


parseRule : String -> String -> String
parseRule sym prod = 
    case run prodP prod of
       Ok s ->
        case s of 
            is ->
                "Symbol: " ++ (Debug.toString is)
       Err _ ->
        "Error"
        

-- Parsers

prodP : Parser (List ProdPart)
prodP =
    loop [] prodParser

prodParser : List ProdPart -> Parser (Step (List ProdPart) (List ProdPart))
prodParser revPps = oneOf 
            [ succeed (\pp -> Loop (Token pp :: revPps)) |= (getChompedString (succeed () |. chompWhile (\c -> c /= '#')))
            , succeed (\pp -> Loop (pp :: revPps)) |= prodSymbol]


prodSymbol : Parser ProdPart
prodSymbol = 
        succeed Symbol
        |=  (getChompedString <|
            succeed ()
            |. symbol "#"
            |. chompUntil "#")
    