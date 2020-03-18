module Generator exposing (..)

import Dict exposing (Dict)
import Parser exposing (..)


type ProdPart
    = Symbol String
    | Token String


type Production
    = Terminal (List ProdPart)
    | NonTerminal (List ProdPart)


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
    case p of
        Terminal pp ->
            "T: " ++ (String.join " " <| List.map partPrint pp)

        NonTerminal pp ->
            "NT: " ++ (String.join " " <| List.map partPrint pp)


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
    let
        prodParts : Result (List DeadEnd) (List ProdPart)
        prodParts =
            Parser.run productionParser prod
    in
    case prodParts of
        Ok parsed ->
            if List.any symbol parsed then
                Ok (NonTerminal parsed)

            else
                Ok (Terminal  parsed)

        Err x ->
            Err x


symbol : ProdPart -> Bool
symbol prodPart =
    case prodPart of
        Symbol _ ->
            True

        Token _ ->
            False


parseProd : String -> Result (List DeadEnd) Production
parseProd inp =
    let
        parsed =
            run productionParser inp
    in
    case parsed of
        Ok pps ->
            Ok (NonTerminal pps)

        Err errors ->
            Err errors


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
                    strippedPart = part |> String.dropLeft 1 |> String.dropRight 1
                in
                
                if String.isEmpty strippedPart then
                    problem "Expected at least one character"
                else
                    succeed (Symbol strippedPart)
            )
        

