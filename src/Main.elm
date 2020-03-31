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
import ExampleGrammars as EG
import File exposing (File)
import File.Download as Download
import File.Select as Select
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
import Task


type alias Model =
    { ntValue : String
    , prodValue : String
    , grammar : Grammar
    , error : Maybe (List DeadEnd)
    , output : String
    , seed : Int
    , symbolHovered : Maybe String
    , screen : Element.Device
    , showHelp : Bool
    , symbolFocused : Bool
    , productionHovered : Maybe ( String, Int )
    }


init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init screenSize =
    ( { ntValue = ""
      , prodValue = ""
      , grammar = EG.defaultGrammar
      , screen = Element.classifyDevice screenSize
      , error = Nothing
      , output = "Click 'generate' to generate some text!"
      , seed = 42
      , symbolHovered = Nothing
      , productionHovered = Nothing
      , showHelp = True
      , symbolFocused = False
      }
    , Cmd.none
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
    | ExitSymbolHover
    | ScreenResize Int Int
    | SymbolFocus
    | SymbolLoseFocus
    | SymbolSelected String
    | ProductionHover ( String, Int )
    | ExitProductionHover
    | CopyProduction ( String, Int )
    | DeleteProduction ( String, Int )
    | ClickedSave
    | ClickedLoad
    | JsonSelected File
    | JsonLoaded String


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

        CopyProduction ( sym, ix ) ->
            ( { model
                | prodValue =
                    Maybe.withDefault model.ntValue <|
                        Maybe.map Grammar.prodToString <|
                            Grammar.getRule model.grammar sym ix
              }
            , Cmd.none
            )

        DeleteProduction ( sym, ix ) ->
            ( { model | grammar = Grammar.removeRule model.grammar sym ix }, Cmd.none )

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
            ( { model | symbolHovered = Just sym }, Cmd.none )

        ExitSymbolHover ->
            ( { model | symbolHovered = Nothing }, Cmd.none )

        ScreenResize w h ->
            ( { model | screen = Element.classifyDevice { width = w, height = h } }, Cmd.none )

        SymbolFocus ->
            ( { model | symbolFocused = True }, Cmd.none )

        SymbolLoseFocus ->
            ( { model | symbolFocused = False }, Cmd.none )

        SymbolSelected sym ->
            ( { model | ntValue = sym }, Cmd.none )

        ProductionHover ( sym, ix ) ->
            case model.symbolHovered of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( { model | productionHovered = Just ( sym, ix ) }, Cmd.none )

        ExitProductionHover ->
            ( { model | productionHovered = Nothing }, Cmd.none )

        ClickedSave ->
            ( model, Download.string "grammar.json" "application/json" <| JE.encode 4 (grammarEncoder model.grammar) )

        ClickedLoad ->
            ( model, Select.file [ "application/json" ] JsonSelected )

        JsonSelected file ->
            ( model, Task.perform JsonLoaded (File.toString file) )

        JsonLoaded grammar ->
            case JD.decodeString grammarDecoder grammar of
                Ok gram ->
                    ( { model | grammar = gram }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        columnWidth =
            case model.screen.orientation of
                Portrait ->
                    case model.screen.class of
                        Element.Phone ->
                            Element.px 325

                        _ ->
                            fill

                Landscape ->
                    case model.screen.class of
                        Element.Phone ->
                            fill

                        Element.Desktop ->
                            Element.px 1140

                        Element.BigDesktop ->
                            Element.px 1140

                        _ ->
                            Element.px 1024
    in
    Element.layout
        [ Font.family
            [ Font.sansSerif
            ]
        , width fill
        , Background.color dblue
        ]
    <|
        Element.column
            [ width columnWidth
            , centerX
            , Element.paddingEach { top = 80, left = 0, right = 0, bottom = 0 }
            ]
            [ titleView
            , grammarView model.grammar model.symbolHovered model.productionHovered
            , inputView model
            , outputView model
            , el
                [ height Element.shrink
                , Element.alignTop
                , width fill
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
    Element.column [ width fill, centerX ]
        [ el
            [ Element.paddingEach { top = 10, left = 0, right = 0, bottom = 10 }
            , Font.bold
            , Font.size 32
            , width fill
            , Font.center
            , Font.color white
            , centerX
            ]
          <|
            Element.text "Context Free Goblin"
        , el
            [ Element.paddingEach { top = 10, left = 5, right = 0, bottom = 45 }
            , width fill
            , Font.center
            ]
          <|
            Element.paragraph
                [ Font.size 16
                , Font.color goblinGreen
                ]
                [ Element.text "A tool for building and sharing random text generators." ]
        ]


textInputStyle : List (Element.Attribute Msg)
textInputStyle =
    [ Font.size 12
    , Border.rounded 0
    ]


symbolInputDropdown : Model -> Element Msg
symbolInputDropdown model =
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
                    , Events.onMouseLeave ExitSymbolHover
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
                    symbolInputDropdown model

            else
                Element.below Element.none
    in
    Element.row
        [ width fill
        , Background.color dblack
        , Element.paddingXY 10 10

        -- , height fill
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
                        Input.labelBelow [ Element.paddingXY 0 10 ] <|
                            Element.paragraph
                                [ Font.color helpWhite
                                , Font.size 13
                                , Font.family
                                    [ Font.sansSerif
                                    ]
                                , Element.spacing 10
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
                textInputStyle
                { onChange = ProdChange
                , label =
                    if model.showHelp then
                        Input.labelBelow [ Element.paddingXY 0 10 ] <|
                            Element.paragraph
                                [ Font.color helpWhite
                                , Font.size 13
                                , Font.family [ Font.sansSerif ]
                                , Element.spacing 10
                                ]
                            <|
                                [ Element.text "Put some text here, use {} to mark symbols." ]

                    else
                        Input.labelHidden "Put some text here, use {} to delimit symbols"
                , placeholder = Just <| Input.placeholder [] (Element.text "You see a {monster}")
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
        , spacing 30
        , Element.alignTop
        , Element.padding 30
        ]
        [ el
            [ width fill
            , centerX
            , Element.clipX
            , Element.scrollbarX
            , Background.color lDark
            , Border.rounded 5
            ]
          <|
            Element.paragraph
                [ centerY
                , Font.center
                , padding 10
                , Font.color white
                , Font.size 28
                ]
                [ Element.text model.output ]
        , Input.button
            [ centerX
            , Background.color goblinGreen
            , Border.rounded 5
            , Element.paddingEach { top = 10, bottom = 8, left = 65, right = 65 }
            , Element.mouseDown
                [ Background.color goblinGreen ]
            , Element.mouseOver
                [ Background.color goblinGreen
                , Border.color dblack
                , Font.color dblack
                ]
            , Font.color white
            , Font.center
            ]
            { onPress = Just Generate, label = Element.text "GENERATE" }
        , Element.el
            [ width fill
            , centerX
            , Font.center
            , Font.color helpWhite
            , Font.size 12
            ]
          <|
            Element.text "- OR -"
        , Element.row
            [ centerY
            , centerX
            , spacing 10
            ]
          <|
            [ Input.button
                [ Background.color dblack
                , Element.paddingEach { top = 8, bottom = 6, left = 15, right = 15 }
                , Border.color goblinGreen
                , Border.rounded 5
                , Border.width 1
                , Element.mouseDown
                    [ Background.color goblinGreen ]
                , Element.mouseOver
                    [ Background.color goblinGreen
                    , Border.color dblack
                    , Font.color dblack
                    ]
                , Font.size 20
                , Font.color goblinGreen
                , Font.center
                ]
                { onPress = Just ClickedSave, label = Element.text "save" }
            , Input.button
                [ Background.color dblack
                , Element.paddingEach { top = 8, bottom = 6, left = 15, right = 15 }
                , Border.color goblinGreen
                , Border.rounded 5
                , Border.width 1
                , Element.mouseDown
                    [ Background.color goblinGreen ]
                , Element.mouseOver
                    [ Background.color goblinGreen
                    , Border.color dblack
                    , Font.color dblack
                    ]
                , Font.size 20
                , Font.color goblinGreen
                , Font.center
                ]
                { onPress = Just ClickedLoad, label = Element.text "load" }
            ]
        ]



-- GRAMMAR VIEW


type alias GrammarRecord =
    { symbol : String
    , productions : List Production
    }


grammarRecords : Grammar -> List GrammarRecord
grammarRecords (Grammar rules) =
    Dict.toList rules |> List.map (\( sym, prods ) -> GrammarRecord sym prods)


hoveredSymbolStyle : List (Element.Attribute Msg)
hoveredSymbolStyle =
    [ Font.underline
    , Background.color goblinGreen
    , Font.color dblack
    ]


grammarRow : Int -> Maybe String -> Maybe ( String, Int ) -> GrammarRecord -> Element Msg
grammarRow i hoveredSymbol hoveredProduction production =
    let
        hoveredRowStyle =
            case hoveredSymbol of
                Nothing ->
                    [ Background.color <| rowColor i ]

                Just hoveredSym ->
                    if hoveredSym == production.symbol then
                        [ Background.color goblinGreen ]

                    else
                        [ Background.color <| rowColor i ]
    in
    Element.row
        (width fill
            :: hoveredRowStyle
        )
    <|
        [ el
            [ width <| fillPortion 1
            , height fill
            , Element.clipX
            ]
          <|
            el
                [ width fill
                , height fill
                , Events.onMouseEnter (SymbolHover production.symbol)
                , Events.onMouseLeave ExitSymbolHover
                ]
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
                    [ Element.text production.symbol ]
        , el
            [ width <| fillPortion 5
            , height fill
            ]
          <|
            Element.wrappedRow
                [ Element.spacingXY 5 5
                , padding 10
                ]
            <|
                List.indexedMap (renderProduction production.symbol hoveredSymbol hoveredProduction) production.productions
        ]


headerRow : Element Msg
headerRow =
    Element.row
        [ width fill
        , Background.color dblack
        , Element.paddingEach { bottom = 3, top = 3, left = 0, right = 10 }
        ]
        [ el
            [ width <| fillPortion 1
            , height fill
            , Element.clipX
            ]
          <|
            el
                [ Background.color dblack
                , height fill
                , width fill
                , Font.color white
                , Font.size 16
                , padding 15
                ]
            <|
                Element.paragraph
                    [ centerX
                    , centerY
                    , Font.center
                    , Font.light
                    ]
                    [ Element.text "SYMBOL" ]
        , el
            [ width <| fillPortion 3
            , height fill
            ]
          <|
            el
                [ Background.color dblack
                , height fill
                , width fill
                , Font.color white
                , Font.size 16
                , padding 15
                ]
            <|
                Element.paragraph
                    [ centerX
                    , centerY
                    , Font.alignLeft
                    , Font.light
                    ]
                    [ Element.text "RULES" ]
        , el
            [ width <| fillPortion 2
            , height fill
            , Element.alignRight
            ]
          <|
            Input.button
                [ Element.alignRight
                , Background.color dblack
                , Element.paddingEach { bottom = 5, left = 7, right = 7, top = 8 }
                , Border.color white
                , Border.width 1
                , Element.centerY
                , Element.mouseDown
                    [ Background.color goblinGreen ]
                , Font.color white
                , Font.size 16
                ]
                { onPress = Just ClearGrammar, label = Element.text "X CLEAR ALL" }

        -- Element.paragraph
        --     [ padding 5
        --     , Font.color white
        --     , Border.color white
        --     , Border.width 1
        --     , Font.center
        --     , width Element.shrink
        --     ]
        -- <|
        --     [  ]
        ]


grammarView : Grammar -> Maybe String -> Maybe ( String, Int ) -> Element Msg
grammarView gram symbolHovered productionHovered =
    let
        ruleRecords =
            grammarRecords gram
    in
    Element.column
        [ Background.color lgrey
        , width fill
        , height <| Element.minimum 380 <| Element.shrink
        , Element.scrollbarY
        , Element.clipX
        ]
    <|
        headerRow
            :: List.indexedMap
                (\i gr -> grammarRow i symbolHovered productionHovered gr)
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
                    , Events.onMouseLeave ExitSymbolHover
                    , Font.color goblinGreen
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


productionDropDown : ( String, Int ) -> Element Msg
productionDropDown ( sym, ix ) =
    Element.column
        [ Element.alignRight
        , Font.center
        , Font.size 12
        , Background.color dblack
        , Border.color white
        , spacing 5
        ]
        [ el
            [ Element.mouseOver
                [ Background.color goblinGreen
                ]
            , Element.pointer
            , width fill
            , height fill
            , padding 5
            , Events.onMouseDown (DeleteProduction ( sym, ix ))
            ]
          <|
            Element.text "Remove"
        , el
            [ Element.mouseOver
                [ Background.color goblinGreen
                ]
            , Element.pointer
            , width fill
            , padding 5
            , height fill
            , Events.onMouseDown (CopyProduction ( sym, ix ))
            ]
          <|
            Element.text "Copy"
        ]


productionStyle : String -> Int -> Maybe ( String, Int ) -> List (Element.Attribute Msg)
productionStyle sym ix hoveredProduction =
    let
        default =
            [ Element.paddingXY 10 5
            , Background.color dblack
            , Border.rounded 5
            , Font.color white
            , Font.size 14
            , Events.onMouseEnter <| ProductionHover ( sym, ix )
            , Events.onMouseLeave ExitProductionHover
            ]

        hovered =
            [ Element.paddingXY 10 5
            , Background.color dblack
            , Font.color white
            , Font.size 14
            , Events.onMouseEnter <| ProductionHover ( sym, ix )
            , Events.onMouseLeave ExitProductionHover
            , Element.below <| productionDropDown ( sym, ix )
            ]
    in
    case hoveredProduction of
        Nothing ->
            default

        Just ( sym_, ix_ ) ->
            if (sym == sym_) && (ix == ix_) then
                hovered

            else
                default


renderProduction : String -> Maybe String -> Maybe ( String, Int ) -> Int -> Production -> Element Msg
renderProduction symbol symbolHovered productionHovered ix prod =
    Element.row
        (productionStyle symbol ix productionHovered)
    <|
        List.map (renderProdPart symbolHovered) prod


rowColor : Int -> Element.Color
rowColor ix =
    if modBy 2 ix == 0 then
        lgrey

    else
        mgrey



-- List.map (\chars -> Element.text <| chars) <|
-- String.split "" sym


black : Element.Color
black =
    Element.rgba255 0 0 0 1


dblack : Element.Color
dblack =
    Element.rgba255 45 45 45 1


grey : Element.Color
grey =
    Element.rgba255 129 141 146 1


lgrey : Element.Color
lgrey =
    Element.rgba255 238 238 238 1


mgrey : Element.Color
mgrey =
    Element.rgba255 212 212 212 1


white : Element.Color
white =
    Element.rgba255 255 255 255 1


helpWhite : Element.Color
helpWhite =
    Element.rgba255 255 255 255 0.7


goblinGreen : Element.Color
goblinGreen =
    -- Element.rgba255 0 250 154 1
    Element.rgba255 0 216 133 1


lDark : Element.Color
lDark =
    Element.rgba255 41 41 41 1


dblue : Element.Color
dblue =
    Element.rgba255 1 22 40 1
