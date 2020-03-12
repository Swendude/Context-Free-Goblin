module Main exposing (..)

import Browser
import Element exposing (Element, el, text, row, alignRight, fill, width, rgb255, spacing, centerY, padding, fillPortion, centerX)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generator exposing (..)
import Dict exposing (Dict)
import Debug

type alias Model = 
    { ntValue : String
    , prodValue : String
    , grammar : Grammar
    , ph : String
    }

init : Model
init = 
    { ntValue = "" 
    , prodValue = ""
    , grammar = Generator.empty
    , ph = ""}

main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = NTermChange String
    | ProdChange String
    | Save


update msg model =
    case msg of
        NTermChange v ->
            { model | ntValue = v }
        ProdChange v ->
            { model | prodValue = v }
        Save ->
            { model | grammar = addRule model.grammar model.ntValue model.prodValue, ph = Debug.log (parseRule "" model.prodValue) ""  }

-- VIEW

blue = Element.rgb 0.1 0.51 1

lblue = Element.rgba 0.1 0.51 1 0.7

rowStyle : List (Element.Attribute msg)
rowStyle = [width fill, spacing 10, padding 10]


header : Element msg
header = Element.row rowStyle [el [width <| fillPortion 1, Background.color blue] (Element.text "Symbol")
                                                          , el [width <| fillPortion 5] (Element.text "Rule")]

renderProduction : String -> String -> Element msg
renderProduction nt prod =
    Element.row rowStyle [el [width <| fillPortion 1, Background.color lblue] (Element.text nt)
                   , el [width <| fillPortion 5] (Element.text prod) ]

renderProductions : String -> List String -> List (Element msg) -> List (Element msg)
renderProductions nt prods acc =
    let 
        first =
            case List.head prods of
            Just prod ->
                renderProduction nt prod
            Nothing ->
                Element.none
        rest = Maybe.withDefault [] (List.tail prods)
    in
    first :: List.map (renderProduction "")  rest ++ acc
    

grammrows : Grammar -> List (Element msg)
grammrows gram = 
    case gram of
        Grammar rules ->
            Dict.foldl renderProductions [] rules

inputRows : Element Msg            
inputRows = Element.row rowStyle [ Input.text [width <| fillPortion 1] {onChange = NTermChange
                                            , label = Input.labelHidden "Symbol"
                                            , placeholder = Just <| Input.placeholder [] (Element.text "Symbol")
                                            , text = "" } 
                            , Input.text [width <| fillPortion 4] {onChange = ProdChange
                                            , label = Input.labelHidden "Production"
                                            , placeholder = Just <| Input.placeholder []  (Element.text "Production")
                                            , text = "" } 
                            , Input.button [width <| fillPortion 1] {onPress= Just Save, label = Element.text "Add Rule"}]

view model =
    Element.layout [] <| Element.column [width fill] <| header :: (List.append (grammrows model.grammar) [inputRows])