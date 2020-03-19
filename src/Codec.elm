module Codec exposing (grammarDecoder, grammarEncoder, jsonGrammarRules, prodDecoder, prodEncoder, prodPartEncoder, prodPartFromType, prodsDecoder, ruleDecoder, rulesDecoder, rulesEncoder, symbolDecoder, tokenDecoder)

import Dict exposing (Dict)
import Grammar exposing (Grammar(..), ProdPart(..), Production)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


grammarDecoder : Decoder Grammar
grammarDecoder =
    JD.map Grammar <| JD.field "rules" rulesDecoder


rulesDecoder : Decoder (Dict String (List Production))
rulesDecoder =
    JD.map Dict.fromList <| ruleDecoder


ruleDecoder : Decoder (List ( String, List Production ))
ruleDecoder =
    JD.list <| JD.map2 Tuple.pair (JD.field "symbol" JD.string) (JD.field "productions" prodsDecoder)


prodsDecoder : Decoder (List Production)
prodsDecoder =
    JD.list prodDecoder


prodDecoder : Decoder (List ProdPart)
prodDecoder =
    JD.field "type" JD.string
        |> JD.andThen prodPartFromType
        |> JD.list


prodPartFromType : String -> Decoder ProdPart
prodPartFromType str =
    case str of
        "symbol" ->
            symbolDecoder

        "token" ->
            tokenDecoder

        _ ->
            JD.fail ("Invalid type: " ++ str)


tokenDecoder : Decoder ProdPart
tokenDecoder =
    JD.map Token (JD.field "str" JD.string)


symbolDecoder : Decoder ProdPart
symbolDecoder =
    JD.map Symbol (JD.field "str" JD.string)


jsonGrammarRules =
    Dict.fromList
        [ ( "START", [ [ Token "He saw a ", Symbol "animal" ] ] )
        , ( "animal", [ [ Token "dog" ], [ Token "sheep" ] ] )
        ]


grammarEncoder : Grammar -> Value
grammarEncoder (Grammar rules) =
    JE.object
        [ ( "rules", JE.list rulesEncoder (Dict.toList rules) )
        ]


rulesEncoder : ( String, List Production ) -> Value
rulesEncoder ( sym, prods ) =
    JE.object
        [ ( "symbol", JE.string sym )
        , ( "productions", JE.list prodEncoder prods )
        ]


prodEncoder : Production -> Value
prodEncoder prod =
    JE.list prodPartEncoder prod


prodPartEncoder : ProdPart -> Value
prodPartEncoder prodPart =
    case prodPart of
        Symbol str ->
            JE.object
                [ ( "type"
                  , JE.string "symbol"
                  )
                , ( "str"
                  , JE.string str
                  )
                ]

        Token str ->
            JE.object
                [ ( "type"
                  , JE.string "token"
                  )
                , ( "str"
                  , JE.string str
                  )
                ]
