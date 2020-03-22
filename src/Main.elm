module Main exposing (..)

import Browser
import Codec exposing (grammarDecoder, grammarEncoder)
import Debug
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerX, centerY, el, fill, fillPortion, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import ExampleGrammars
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ntValue = ""
      , prodValue = ""
      , grammar =
            Grammar Dict.empty
      , error = Nothing
      , output = "Click 'generate' to generate some text!"
      , seed = 42
      , hovered = Nothing
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
    Sub.none


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


dblack =
    Element.rgba255 45 45 45 1


grey =
    Element.rgba255 129 141 146 1


lgrey =
    Element.rgba255 237 237 237 1


brown =
    Element.rgba255 165 117 72 1


white =
    Element.rgba255 255 255 255 1


mgrey =
    Element.rgba255 185 186 184 1


inputRows : Model -> List (Element Msg)
inputRows model =
    [ Input.text [ Element.alignTop, width <| fillPortion 1, Border.rounded 0, Border.width 0 ]
        { onChange = NTermChange
        , label =
            Input.labelBelow [] <|
                Element.paragraph [ Font.color white, Font.size 12 ] <|
                    [ Element.text "Put a symbol here, there should always be a START symbol" ]
        , placeholder = Just <| Input.placeholder [] (Element.text "START")
        , text = model.ntValue
        }
    , Input.text [ Element.alignTop, width <| fillPortion 3, Border.rounded 0, Border.width 0 ]
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


grammarView : Grammar -> Maybe String -> Element Msg
grammarView gram hovered =
    let
        ruleRecords =
            grammarRecords gram
    in
    el
        [ Background.color mgrey
        , width fill
        , height <| Element.maximum 500 <| fillPortion 5
        , Element.clipY
        , Element.scrollbarY
        ]
    <|
        Element.indexedTable
            [ Element.spacingXY 0 5
            , width fill
            ]
            { data = ruleRecords
            , columns =
                [ { header = renderHeader "SYMBOL"
                  , width = Element.fillPortion 1
                  , view = \i rule -> renderSymbolCol hovered i rule.symbol
                  }
                , { header = renderHeader "RULES"
                  , width = Element.fillPortion 5
                  , view =
                        \i rule -> renderProductionsCol hovered i rule.productions
                  }
                ]
            }


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
                    ]
            in
            case hovered of
                Just sym ->
                    if sym == production then
                        el
                            (defaultSymbolStyle
                                ++ [ Font.underline
                                   , Font.color grey
                                   ]
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
        [ Border.width 0
        , Border.rounded 5
        , Element.paddingEach { bottom = 0, top = 0, left = 10, right = 10 }
        , Background.color dblack
        , Font.color white
        ]
    <|
        List.map (renderProdPart hovered) prod


renderHeader : String -> Element Msg
renderHeader label =
    el
        [ Background.color dblack
        , height fill
        , width Element.shrink
        , Font.color white
        , padding 10
        ]
    <|
        Element.paragraph
            [ centerX
            , centerY
            , Font.center
            , Font.light
            ]
            [ Element.text label ]


renderProductionsCol : Maybe String -> Int -> List Production -> Element Msg
renderProductionsCol hovered i prods =
    let
        rowcolor =
            Background.color <|
                if modBy 2 i == 0 then
                    mgrey

                else
                    lgrey
    in
    Element.wrappedRow
        [ rowcolor
        , Element.spacingXY 5 5
        , padding 10
        ]
    <|
        List.map (renderProduction hovered) prods


renderSymbolCol : Maybe String -> Int -> String -> Element Msg
renderSymbolCol hovered i sym =
    let
        rowcolor =
            case hovered of
                Nothing ->
                    Background.color <|
                        if modBy 2 i == 0 then
                            mgrey

                        else
                            lgrey

                Just hoveredSym ->
                    if sym == hoveredSym then
                        Background.color grey

                    else
                        Background.color <|
                            if modBy 2 i == 0 then
                                mgrey

                            else
                                lgrey
    in
    el
        [ rowcolor
        , height fill
        , width Element.shrink
        , Events.onMouseEnter (SymbolHover sym)
        , Events.onMouseLeave ExitHover
        ]
    <|
        Element.paragraph
            [ centerX
            , centerY
            , Font.center
            , Font.bold
            ]
            [ Element.text sym ]


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ width fill, height fill ]
            [ el
                [ Font.center
                , Font.light
                , Font.color white
                , width fill
                , padding 28
                , Font.size 24
                , Background.color black
                ]
                (Element.text "Context Free Goblin")
            , grammarView model.grammar model.hovered
            , Element.row
                [ Background.color black
                , height <| fillPortion 1
                , width fill
                , spacing 5
                , padding 10
                ]
              <|
                inputRows model
            , el
                [ Background.color black
                , width fill
                , spacing 5
                , padding 10
                ]
              <|
                Element.paragraph
                    [ Background.color white
                    , Font.color black
                    , width <| Element.px 500
                    , padding 20
                    , Font.italic
                    , Font.center
                    , Font.light
                    , centerX
                    ]
                    [ Element.text model.output ]
            , el
                [ Background.color black
                , height <| fillPortion 1
                , width fill
                , spacing 5
                , padding 10
                , Font.color white
                , Font.italic
                , Font.center
                , Font.light
                ]
              <|
                Element.paragraph
                    []
                    []
            ]
