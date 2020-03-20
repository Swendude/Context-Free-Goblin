module Main exposing (Model, Msg(..), blue, grammrows, header, init, inputRows, lblue, main, renderProduction, renderProductions, update, view)

import Browser
import Codec exposing (grammarDecoder, grammarEncoder)
import Debug
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerX, centerY, el, fill, fillPortion, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ntValue = ""
      , prodValue = ""
      , grammar =
            Grammar Dict.empty
      , error = Nothing
      , output = ""
      , seed = 42
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



-- GraphQl
{-
   - Default -> Get the mother grammar
   - Save grammar
   - Load grammar
-}
-- type alias SavedGrammar =
--     { id : String
--     , name : String
--     , json_grammar : String
--     , parent : String
--     , description : String
--     }


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



-- makeSavedGrammars : String -> String -> Maybe SavedGrammar
-- makeSavedGrammars =
--     Maybe.map SavedGrammar


defaultGrammar : SelectionSet (Maybe SavedGrammar) RootQuery
defaultGrammar =
    Query.grammars_by_pk { id = Uuid "00000000-0000-0000-0000-000000000000" } <|
        SelectionSet.map2 SavedGrammar
            Grammars.name
            Grammars.grammar



-- Query.grammars (\optionals -> { optionals | id = "00000000-0000-0000-0000-000000000000" }) <|
-- Query.grammars (\optionals -> { optionals | where_= Present 5 })
-- {id = "00000000-0000-0000-0000-000000000000"} SavedGrammar
-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NTermChange v ->
            ( { model | ntValue = v }, Cmd.none )

        ProdChange v ->
            ( { model | prodValue = v }, Cmd.none )

        Save ->
            if model.ntValue /= "" || model.prodValue /= "" then
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


underline : Element.Attribute msg
underline =
    Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }


header : List (Element msg)
header =
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


renderProduction : String -> Production -> Element msg
renderProduction nt prod =
    Element.row [ width <| fill ]
        [ el [ width <| fillPortion 1, padding 10, underline ] <| renderSymbol nt
        , el [ width <| fillPortion 5, padding 10, underline ] <| Element.row [] <| List.map renderProdpart prod
        ]


renderProductions : String -> List Production -> List (Element msg) -> List (Element msg)
renderProductions nt prods acc =
    List.map (renderProduction nt) prods ++ acc


grammrows : Grammar -> List (Element msg)
grammrows gram =
    case gram of
        Grammar rules ->
            Dict.foldl renderProductions [] rules


inputRows : Model -> List (Element Msg)
inputRows model =
    [ Input.text [ width <| fillPortion 1, Border.rounded 0, Border.width 0 ]
        { onChange = NTermChange
        , label = Input.labelHidden "Symbol"
        , placeholder = Just <| Input.placeholder [] (Element.text "Symbol")
        , text = model.ntValue
        }
    , Input.text [ width <| fillPortion 3, Border.rounded 0, Border.width 0 ]
        { onChange = ProdChange
        , label = Input.labelHidden "Production"
        , placeholder = Just <| Input.placeholder [] (Element.text "Production")
        , text = model.prodValue
        }
    , Input.button
        [ width <| fillPortion 1
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
        [ width <| fillPortion 1
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
        [ width <| fillPortion 1
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


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ width fill, height fill ]
            [ el
                [ Font.center
                , Font.light
                , Font.color white
                , width fill
                , padding 10
                , Font.size 24
                , Background.color black
                ]
                (Element.text "Context Free Goblin")
            , Element.row
                [ height <| Element.px 30
                , width fill
                , padding 10
                , Background.color black
                , Font.color white
                ]
                header
            , Element.row [ width fill ] <|
                [ Element.column
                    [ Border.solid
                    , Border.width 2
                    , Background.color lgrey
                    , height <| Element.px 500
                    , Element.clipY
                    , Element.scrollbarY
                    , Element.width fill
                    ]
                    (grammrows model.grammar)
                ]
            , Element.row
                [ Background.color black
                , height <| Element.px 60
                , width fill
                , spacing 5
                , padding 10
                ]
              <|
                inputRows model
            , el
                [ Background.color black
                , height <| Element.px 60
                , width fill
                , spacing 5
                , padding 10
                , Font.color white
                , Font.italic
                , Font.center
                , Font.light
                ]
              <|
                Element.text model.output
            ]
