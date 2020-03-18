module Main exposing (Model, Msg(..), blue, grammrows, header, init, inputRows, lblue, main, renderProduction, renderProductions, update, view)

import Browser
import Debug
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerX, centerY, el, fill, fillPortion, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Grammar exposing (..)
import Parser exposing (DeadEnd, run)


type alias Model =
    { ntValue : String
    , prodValue : String
    , grammar : Grammar
    , error : Maybe (List DeadEnd)
    }


init : Model
init =
    { ntValue = ""
    , prodValue = ""
    , grammar =
        Grammar <|
            Dict.fromList
                [ ( "Start", [ [ Token "He saw a ", Symbol "animal", Token "in a ", Symbol "location" ] ] )
                , ( "animal", [ [ Token "dog" ], [ Token "sheep" ] ] )
                , ( "location", [ [ Token "forest" ], [ Token "city" ] ] )
                ]
    , error = Nothing
    }


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = NTermChange String
    | ProdChange String
    | Save


update : Msg -> Model -> Model
update msg model =
    case msg of
        NTermChange v ->
            { model | ntValue = v }

        ProdChange v ->
            { model | prodValue = v }

        Save ->
            case addRule model.grammar model.ntValue model.prodValue of
                Ok gram ->
                    { model
                        | grammar = gram
                        , prodValue = ""
                    }

                Err error ->
                    { model | error = Just error }



-- VIEW


blue =
    Element.rgb 0.1 0.51 1


lblue =
    Element.rgba 0.1 0.51 1 0.7


red =
    Element.rgba255 208 0 0 1


yellow =
    Element.rgba255 255 186 8 1


black =
    Element.rgba255 49 62 80 1


grey =
    Element.rgba255 129 141 146 1


lgrey =
    Element.rgba255 129 141 146 0.1


brown =
    Element.rgba255 165 117 72 1


white =
    Element.rgba255 255 255 255 1


header : Element msg
header =
    Element.row [ width fill, spacing 10, padding 10, Background.color black, Font.color white ]
        [ el [ width <| fillPortion 1, padding 10 ] (Element.text "Symbol")
        , el [ width <| fillPortion 5, padding 10 ] (Element.text "Rule")
        ]


renderProdpart : ProdPart -> Element msg
renderProdpart pp =
    case pp of
        Symbol production ->
            renderSymbol production

        Token production ->
            el [ padding 10 ] (Element.text production)


renderSymbol : String -> Element msg
renderSymbol sym =
    el [ Background.color grey, padding 10, Border.rounded 5, Font.color white ] (Element.text sym)


renderProduction : String -> Int -> Production -> Element msg
renderProduction nt i prod =
    let
        prodColStyle =
            case modBy 2 i of
                0 ->
                    [ width <| fillPortion 1, Background.color white, padding 10 ]

                _ ->
                    [ width <| fillPortion 1, Background.color lgrey, padding 10 ]

        ruleColStyle =
            case modBy 2 i of
                0 ->
                    [ width <| fillPortion 5, Background.color white, padding 10 ]

                _ ->
                    [ width <| fillPortion 5, Background.color lgrey, padding 10 ]
    in
    Element.row [ width <| fill ]
        [ el prodColStyle <| renderSymbol nt
        , el ruleColStyle <| Element.row [] <| List.map renderProdpart prod
        ]


renderProductions : String -> List Production -> List (Element msg) -> List (Element msg)
renderProductions nt prods acc =
    List.indexedMap (renderProduction nt) prods ++ acc


grammrows : Grammar -> List (Element msg)
grammrows gram =
    case gram of
        Grammar rules ->
            Dict.foldl renderProductions [] rules


inputRows : Model -> Element Msg
inputRows model =
    Element.row [ width <| fill, spacing 5, padding 10 ]
        [ Input.text [ width <| fillPortion 1 ]
            { onChange = NTermChange
            , label = Input.labelHidden "Symbol"
            , placeholder = Just <| Input.placeholder [] (Element.text "Symbol")
            , text = model.ntValue
            }
        , Input.text [ width <| fillPortion 4 ]
            { onChange = ProdChange
            , label = Input.labelHidden "Production"
            , placeholder = Just <| Input.placeholder [] (Element.text "Production")
            , text = model.prodValue
            }
        , Input.button [ width <| fillPortion 1 ] { onPress = Just Save, label = Element.text "Add Rule" }
        ]


view model =
    Element.layout [] <| Element.column [ width fill ] <| header :: List.append (grammrows model.grammar) [ inputRows model ]
