module Main exposing (..)

import Browser
import Browser.Events
import Codec exposing (grammarDecoder, grammarEncoder)
import Debug
import Dict exposing (Dict)
import Element exposing (Element, Orientation(..), alignRight, centerX, centerY, el, fill, fillPortion, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Grammar exposing (..)
import Grammar.Object.Grammars as Grammars
import Grammar.Query as Query
import Grammar.Scalar exposing (Id(..), Uuid(..))
import Graphql.Http as GqlHttp
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Json.Decode as JD
import Json.Encode as JE
import Parser exposing (DeadEnd, run)
import Random


type alias Model =
    { ntValue : String
    , prodValue : String
    , grammar : Grammar
    , error : Maybe (List DeadEnd)
    , output : String
    , seed : Int
    , hovered : Maybe String
    , screen : Element.Device
    , showHelp : Bool
    , symbolFocused : Bool
    }


init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init screenSize =
    ( { ntValue = ""
      , prodValue = ""
      , grammar =
            Grammar Dict.empty
      , screen = Element.classifyDevice screenSize
      , error = Nothing
      , output = "Click 'generate' to generate some text!"
      , seed = 42
      , hovered = Nothing
      , showHelp = True
      , symbolFocused = False
      }
    , requestDefaultGrammar
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize ScreenResize


requestDefaultGrammar : Cmd Msg
requestDefaultGrammar =
    defaultGrammar
        |> GqlHttp.queryRequest "https://context-free-goblin.herokuapp.com/v1/graphql"
        |> GqlHttp.withOperationName "GetDefaultGrammar"
        |> GqlHttp.send GotDefaultGrammar


type alias SavedGrammar =
    { name : String
    , grammar : String
    }


defaultGrammar : SelectionSet (Maybe SavedGrammar) RootQuery
defaultGrammar =
    Query.grammars_by_pk { id = Uuid "00000000-0000-0000-0000-000000000000" } <|
        SelectionSet.map2 SavedGrammar
            Grammars.name
            Grammars.grammar


type Msg
    = NTermChange String
    | ProdChange String
    | Generate
    | Save
    | NewSeed Int
    | ClearGrammar
    | GotDefaultGrammar (Result (GqlHttp.Error (Maybe SavedGrammar)) (Maybe SavedGrammar))
    | SymbolHover String
    | ExitHover
    | ScreenResize Int Int
    | SymbolFocus
    | SymbolLoseFocus
    | SymbolSelected String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NTermChange v ->
            ( { model | ntValue = v }, Cmd.none )

        ProdChange v ->
            ( { model | prodValue = v }, Cmd.none )

        Save ->
            if model.ntValue /= "" && model.prodValue /= "" then
                case addRule model.grammar model.ntValue model.prodValue of
                    Ok gram ->
                        ( { model
                            | grammar = gram
                            , prodValue = ""
                          }
                        , Cmd.none
                        )

                    Err error ->
                        ( { model | error = Just error }, Cmd.none )

            else
                ( model, Cmd.none )

        NewSeed s ->
            ( { model | seed = s, output = Result.withDefault "ERROR" (Grammar.generateSentence (Random.initialSeed s) model.grammar) }, Cmd.none )

        Generate ->
            ( model, Random.generate NewSeed <| Random.int Random.minInt Random.maxInt )

        ClearGrammar ->
            ( { model | grammar = Grammar.empty, ntValue = "START" }, Cmd.none )

        GotDefaultGrammar res ->
            case res of
                Ok grammarString ->
                    case grammarString of
                        Just gramString ->
                            case JD.decodeString grammarDecoder gramString.grammar of
                                Ok gram ->
                                    ( { model | grammar = gram }, Cmd.none )

                                Err err ->
                                    Debug.log (Debug.toString "ENCODE ERROR") ( model, Cmd.none )

                        Nothing ->
                            Debug.log (Debug.toString "NO ROWS ERROR") ( model, Cmd.none )

                Err error ->
                    case error of
                        GqlHttp.GraphqlError pd err ->
                            Debug.log (Debug.toString pd) ( model, Cmd.none )

                        GqlHttp.HttpError err ->
                            case err of
                                GqlHttp.BadPayload str ->
                                    Debug.log (Debug.toString str) ( model, Cmd.none )

                                _ ->
                                    Debug.log (Debug.toString err) ( model, Cmd.none )

        SymbolHover sym ->
            ( { model | hovered = Just sym }, Cmd.none )

        ExitHover ->
            ( { model | hovered = Nothing }, Cmd.none )

        ScreenResize w h ->
            ( { model | screen = Element.classifyDevice { width = w, height = h } }, Cmd.none )

        SymbolFocus ->
            ( { model | symbolFocused = True }, Cmd.none )

        SymbolLoseFocus ->
            ( { model | symbolFocused = False }, Cmd.none )

        SymbolSelected sym ->
            ( { model | ntValue = sym }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        columnWidth =
            case model.screen.orientation of
                Portrait ->
                    case Debug.log "Screen class" model.screen.class of
                        Element.Phone ->
                            600

                        Element.Desktop ->
                            1300

                        _ ->
                            800

                Landscape ->
                    case Debug.log "Screen class" model.screen.class of
                        Element.Phone ->
                            800

                        Element.Desktop ->
                            950

                        Element.BigDesktop ->
                            1450

                        _ ->
                            800
    in
    Element.layout
        [ Font.family
            [ Font.typeface "Libre Baskerville"
            ]
        , width fill
        , height fill
        ]
    <|
        Element.column
            [ width <| Element.px columnWidth
            , height fill
            , centerX
            , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 10, color = dblack }
            ]
            [ titleView
            , grammarView model.grammar model.hovered
            , inputView model
            , outputView model
            , el
                [ width fill
                , padding 10
                , Font.color white
                , Font.center
                , Font.size 10
                , Background.color dblack
                ]
              <|
                Element.paragraph
                    [ Element.alignBottom
                    , Font.family
                        [ Font.typeface "Cutive Mono"
                        ]
                    ]
                    [ Element.text "Context Free Goblin is free open-source software. Made in Elm by @Swendude with <3" ]
            ]


titleView : Element Msg
titleView =
    Element.column [ width fill, Background.color white ]
        [ el
            [ Element.padding 10
            , Font.bold
            , Font.size 24
            , Font.center
            , width fill
            ]
          <|
            Element.text "Context Free Goblin"
        , el
            [ Element.paddingEach { top = 0, left = 0, right = 0, bottom = 13 }
            , Background.color white
            , width fill
            ]
          <|
            Element.paragraph
                [ Font.size 11
                , Font.center
                ]
                [ Element.text "A tool for building and sharing random text generators." ]

        -- , el [ width (fillPortion 4) ]
        --     Element.none
        ]


textInputStyle : List (Element.Attribute Msg)
textInputStyle =
    [ Font.size 12
    , Border.rounded 0
    ]


dropdown : Model -> Element Msg
dropdown model =
    let
        row : String -> Element Msg
        row =
            \sym ->
                el
                    [ Border.width 1
                    , Border.color dblack
                    , width fill
                    , padding 10
                    , Element.mouseOver
                        [ Background.color goblinGreen
                        ]
                    , Events.onMouseDown (SymbolSelected sym)
                    , Events.onMouseEnter (SymbolHover sym)
                    , Events.onMouseLeave ExitHover
                    ]
                <|
                    Element.paragraph
                        [ centerY
                        , Font.size 14
                        , Font.center
                        , Font.family
                            [ Font.typeface "Cutive Mono"
                            ]
                        , Element.pointer
                        ]
                        [ Element.text sym ]
    in
    Element.column
        [ Background.color white
        , width fill
        , height fill
        , Element.scrollbarX
        ]
    <|
        List.map row (List.filter (\sym -> String.startsWith model.ntValue sym) (Grammar.getSymbols model.grammar))


inputView : Model -> Element Msg
inputView model =
    let
        belowSymbolInput =
            if model.symbolFocused then
                Element.below <|
                    dropdown model

            else
                Element.below Element.none
    in
    Element.row
        [ width fill
        , Background.color dblack
        , Element.paddingXY 10 10
        , height fill
        ]
        [ el [ width <| fillPortion 3, Element.alignTop ] <|
            Input.text
                (textInputStyle
                    ++ [ Events.onFocus SymbolFocus
                       , Events.onLoseFocus SymbolLoseFocus
                       , belowSymbolInput
                       , Font.family
                            [ Font.typeface "Cutive Mono"
                            ]
                       ]
                )
                { onChange = NTermChange
                , label =
                    if model.showHelp then
                        Input.labelBelow [] <|
                            Element.paragraph
                                [ Font.color white
                                , Font.size 10
                                , Font.family
                                    [ Font.typeface "Libre Baskerville"
                                    ]
                                ]
                            <|
                                [ Element.text "Put a symbol here, there should always be a START symbol" ]

                    else
                        Input.labelHidden "Put a symbol here, there should always be a START symbol"
                , placeholder =
                    Just <|
                        Input.placeholder
                            []
                            (Element.text "START")
                , text = model.ntValue
                }
        , el
            [ width <| fillPortion 10
            , Element.paddingEach { top = 0, left = 10, right = 0, bottom = 0 }
            , Element.alignTop
            ]
          <|
            Input.text
                (textInputStyle
                    ++ [ Font.family
                            [ Font.typeface "Libre Baskerville"
                            ]
                       ]
                )
                { onChange = ProdChange
                , label =
                    if model.showHelp then
                        Input.labelBelow [] <|
                            Element.paragraph [ Font.color white, Font.size 10 ] <|
                                [ Element.text "Put some text here, use # to mark symbols." ]

                    else
                        Input.labelHidden "Put some text here, use # to delimit symbols"
                , placeholder = Just <| Input.placeholder [] (Element.text "You see a #monster#")
                , text = model.prodValue
                }
        , el
            [ width <| fillPortion 2
            , Element.paddingEach { top = 0, left = 10, right = 0, bottom = 0 }
            , Element.alignTop
            ]
          <|
            Input.button
                [ centerY
                , Background.color dblack
                , padding 7
                , Border.color white
                , Border.width 1
                , Element.mouseDown
                    [ Background.color goblinGreen ]
                , Font.color white
                , Font.center
                ]
                { onPress = Just Save, label = Element.text "+ Add Rule" }
        ]


outputView : Model -> Element Msg
outputView model =
    Element.column
        [ width fill
        , Background.color dblack
        , Element.paddingXY 10 10
        , height fill
        , spacing 20
        ]
        [ el
            [ width (Element.px 800)
            , height fill
            , centerX
            , Element.clipX
            , Element.scrollbarX
            , Background.color white
            ]
          <|
            Element.paragraph [ centerY, Font.center, padding 10 ]
                [ Element.text model.output ]
        , el
            [ centerY
            , centerX
            ]
          <|
            Input.button
                [ Background.color dblack
                , padding 7
                , Border.color white
                , Border.width 1
                , Element.mouseDown
                    [ Background.color goblinGreen ]
                , Font.color white
                , Font.center
                ]
                { onPress = Just Generate, label = Element.text "GENERATE" }
        ]


inputRows : Model -> List (Element Msg)
inputRows model =
    [ Input.text
        [ Element.alignTop
        , width <| fillPortion 2
        , Border.rounded 3
        , Border.width 1
        ]
        { onChange = NTermChange
        , label =
            Input.labelBelow [] <|
                Element.paragraph [ Font.color black, Font.size 10 ] <|
                    [ Element.text "Put a symbol here, there should always be a START symbol" ]
        , placeholder =
            Just <|
                Input.placeholder
                    []
                    (Element.text "START")
        , text = model.ntValue
        }
    , Input.text
        [ Element.alignTop
        , width <| fillPortion 3
        , Border.rounded 3
        , Border.width 1
        ]
        { onChange = ProdChange
        , label =
            Input.labelBelow [] <|
                Element.paragraph [ Font.color white, Font.size 12 ] <|
                    [ Element.text "Put some text here, use # to delimit symbols" ]
        , placeholder = Just <| Input.placeholder [] (Element.text "You see a #monster#")
        , text = model.prodValue
        }
    , Input.button
        [ Element.alignTop
        , width <| fillPortion 1
        , Background.color blue
        , padding 10
        , Border.rounded 5
        , Border.solid
        , Element.mouseOver
            [ Background.color red ]
        , Element.mouseDown
            [ Background.color grey ]
        , Font.color white
        , Font.center
        ]
        { onPress = Just Save, label = Element.text "+ Add Rule" }
    , Input.button
        [ Element.alignTop
        , width <| fillPortion 1
        , Background.color blue
        , padding 10
        , Border.rounded 5
        , Border.solid
        , Element.mouseOver
            [ Background.color red ]
        , Element.mouseDown
            [ Background.color grey ]
        , Font.color white
        , Font.center
        ]
        { onPress = Just Generate, label = Element.text "Generate > " }
    , Input.button
        [ Element.alignTop
        , width <| fillPortion 1
        , Background.color red
        , padding 10
        , Border.rounded 5
        , Border.solid
        , Element.mouseOver
            [ Background.color black ]
        , Element.mouseDown
            [ Background.color grey ]
        , Font.color white
        , Font.center
        ]
        { onPress = Just ClearGrammar, label = Element.text "Clear X " }
    ]



-- GRAMMAR VIEW


type alias GrammarRecord =
    { symbol : String
    , productions : List Production
    }


grammarRecords : Grammar -> List GrammarRecord
grammarRecords (Grammar rules) =
    Dict.toList rules |> List.map (\( sym, prods ) -> GrammarRecord sym prods)


grammarRow : Int -> Element Msg -> Element Msg -> Element Msg
grammarRow i identifier objects =
    Element.row
        [ width fill
        , Background.color <| rowColor i
        ]
        [ el
            [ width <| fillPortion 1
            , height fill
            , Element.clipX
            ]
            identifier
        , el
            [ width <| fillPortion 5
            , height fill
            ]
            objects
        ]


grammarView : Grammar -> Maybe String -> Element Msg
grammarView gram hovered =
    let
        ruleRecords =
            grammarRecords gram
    in
    Element.column
        [ Background.color lgrey
        , width fill
        , height <| Element.px 380
        , Element.scrollbarY
        , Element.clipX
        ]
    <|
        grammarRow 0 (renderHeader "Symbol") (renderHeader "Rules")
            :: List.indexedMap
                (\i gr -> grammarRow i (renderSymbolCol hovered gr.symbol) (renderProductionsCol hovered gr.productions))
                ruleRecords


renderProdPart : Maybe String -> ProdPart -> Element Msg
renderProdPart hovered part =
    let
        partPadding =
            Element.paddingEach
                { bottom = 10
                , top = 10
                , left = 0
                , right = 0
                }
    in
    case part of
        Symbol production ->
            let
                defaultSymbolStyle =
                    [ partPadding
                    , Font.bold
                    , Events.onMouseEnter (SymbolHover production)
                    , Events.onMouseLeave ExitHover
                    , Font.family
                        [ Font.typeface "Cutive Mono"
                        ]
                    , Element.pointer
                    ]
            in
            case hovered of
                Just sym ->
                    if sym == production then
                        el
                            (defaultSymbolStyle
                                ++ hoveredSymbolStyle
                            )
                            (Element.text production)

                    else
                        el
                            defaultSymbolStyle
                            (Element.text production)

                Nothing ->
                    el defaultSymbolStyle (Element.text production)

        Token production ->
            el [ partPadding, Font.light ] (Element.text production)


renderProduction : Maybe String -> Production -> Element Msg
renderProduction hovered prod =
    Element.row
        [ Element.paddingXY 10 5
        , Background.color dblack
        , Font.color white
        , Font.size 14
        ]
    <|
        List.map (renderProdPart hovered) prod


renderHeader : String -> Element Msg
renderHeader label =
    el
        [ Background.color dblack
        , height fill
        , width fill
        , Font.color white
        , Font.size 12
        , padding 15
        ]
    <|
        Element.paragraph
            [ centerX
            , centerY
            , Font.alignLeft
            , Font.light
            ]
            [ Element.text label ]


rowColor : Int -> Element.Color
rowColor ix =
    if modBy 2 ix == 0 then
        white

    else
        lgrey


renderProductionsCol : Maybe String -> List Production -> Element Msg
renderProductionsCol hovered prods =
    Element.wrappedRow
        [ Element.spacingXY 5 5
        , padding 10
        ]
    <|
        List.map (renderProduction hovered) prods


hoveredSymbolStyle : List (Element.Attribute Msg)
hoveredSymbolStyle =
    [ Font.underline
    , Background.color goblinGreen
    , Font.color dblack
    ]


renderSymbolCol : Maybe String -> String -> Element Msg
renderSymbolCol hovered sym =
    let
        underlined =
            case hovered of
                Nothing ->
                    []

                Just hoveredSym ->
                    if hoveredSym == sym then
                        hoveredSymbolStyle

                    else
                        []
    in
    el
        ([ width fill
         , height fill
         , Events.onMouseEnter (SymbolHover sym)
         , Events.onMouseLeave ExitHover
         ]
            ++ underlined
        )
    <|
        Element.paragraph
            [ width fill
            , centerY
            , Font.size 14
            , Font.center
            , Element.paddingXY 5 0
            , Font.family
                [ Font.typeface "Cutive Mono"
                ]
            , Element.pointer
            ]
            [ Element.text sym ]



-- List.map (\chars -> Element.text <| chars) <|
-- String.split "" sym


blue : Element.Color
blue =
    Element.rgb 0.1 0.51 1


lblue : Element.Color
lblue =
    Element.rgba 0.1 0.51 1 0.7


red : Element.Color
red =
    Element.rgba255 208 0 0 1


yellow : Element.Color
yellow =
    Element.rgba255 255 186 8 1


black : Element.Color
black =
    Element.rgba255 49 62 80 1


dblack : Element.Color
dblack =
    Element.rgba255 45 45 45 1


grey : Element.Color
grey =
    Element.rgba255 129 141 146 1


lgrey : Element.Color
lgrey =
    Element.rgba255 237 237 237 1


brown : Element.Color
brown =
    Element.rgba255 165 117 72 1


white : Element.Color
white =
    Element.rgba255 255 255 255 1


mgrey : Element.Color
mgrey =
    Element.rgba255 185 186 184 1


goblinGreen : Element.Color
goblinGreen =
    Element.rgba255 128 173 160 1
