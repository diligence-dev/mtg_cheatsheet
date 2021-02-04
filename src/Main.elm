module Main exposing (..)

import Browser
import Debug
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Attribute, Html, button, div, form, img, input, text)
import Html.Attributes exposing (disabled, draggable, height, src, style, value, width)
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task


log =
    Debug.log "stuff"


main =
    Browser.element { init = init, subscriptions = always Sub.none, update = update, view = view }


type alias Element =
    { left : Int
    , top : Int
    , imageSource : String
    , zIndex : Int
    , fontSize : Int
    }


encodeElement : Element -> Encode.Value
encodeElement { left, top, zIndex, imageSource, fontSize } =
    Encode.object
        [ ( "left", Encode.int left )
        , ( "top", Encode.int top )
        , ( "imageSource", Encode.string imageSource )
        , ( "zIndex", Encode.int zIndex )
        , ( "fontSize", Encode.int fontSize )
        ]


elementDecoder : Decode.Decoder Element
elementDecoder =
    Decode.map5
        Element
        (Decode.field "left" Decode.int)
        (Decode.field "top" Decode.int)
        (Decode.field "imageSource" Decode.string)
        (Decode.field "zIndex" Decode.int)
        (Decode.field "fontSize" Decode.int)


type alias Model =
    { elements : List Element
    , beingDragged : Maybe ( Element, Int, Int )
    , input : Maybe { left : Int, top : Int, query : String, isLoading : Bool }
    }


nextZIndex : List Element -> Int
nextZIndex elements =
    (elements |> List.map .zIndex |> List.maximum |> Maybe.withDefault 0) + 1


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { elements =
                [ Element 50 50 "https://c1.scryfall.com/file/scryfall-cards/normal/front/6/e/6e4c9574-1ee3-461e-848f-8f02c6a8b7ee.jpg?1594735950" 0 -1
                , Element 500 500 "https://c1.scryfall.com/file/scryfall-cards/normal/front/5/c/5c23869b-c99a-49dd-9e29-fcc0eb63fad1.jpg?1594734879" 1 -1
                , Element 50 500 "snow" 1 40
                ]
            , beingDragged = Nothing
            , input = Nothing
            }
    in
    ( model, Cmd.none )


type Msg
    = DoubleClick Int Int
    | Drag Element Int Int
    | DragEnd
    | DragOver
    | Drop Int Int
    | Input String
    | Submit
    | Response Int Int (Result Http.Error String)
    | Export
    | Import
    | FileLoaded File
    | FileContentLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileContentLoaded fileContent ->
            case Decode.decodeString (Decode.list elementDecoder) fileContent of
                Err _ ->
                    ( model |> Debug.log "error", Cmd.none )

                Ok elements ->
                    ( { model | elements = elements }, Cmd.none )

        FileLoaded file ->
            ( model, Task.perform FileContentLoaded (File.toString file) )

        Import ->
            ( model, Select.file [ "application/json" ] FileLoaded )

        Export ->
            let
                json =
                    Encode.list encodeElement model.elements

                command =
                    Download.string "mtg_cheatsheet.json" "application/json" (Encode.encode 4 json)
            in
            ( model, command )

        Response left top result ->
            case result of
                Err _ ->
                    ( { model | input = model.input |> Maybe.map (\input -> { input | isLoading = False, query = input.query ++ " - not found" }) }, Cmd.none )

                Ok imageSource ->
                    let
                        newModel =
                            { model
                                | input = Nothing
                                , elements = Element left top imageSource 0 -1 :: model.elements
                            }
                    in
                    ( newModel, Cmd.none )

        Submit ->
            case model.input of
                Nothing ->
                    ( model, Cmd.none )

                Just input ->
                    case input.query |> String.words |> List.head |> Maybe.withDefault "" |> String.toInt of
                        Nothing ->
                            if input.query == "" then
                                ( { model | input = Nothing }, Cmd.none )

                            else
                                let
                                    newModel =
                                        { model | input = Just { input | isLoading = True } }

                                    command =
                                        Http.get
                                            { url = "https://api.scryfall.com/cards/search?q=" ++ input.query
                                            , expect =
                                                Http.expectJson
                                                    (Response input.left input.top)
                                                    (Decode.at [ "data", "0", "image_uris", "normal" ] Decode.string)
                                            }
                                in
                                ( newModel, command )

                        Just fontSize ->
                            let
                                elementText =
                                    input.query |> String.words |> List.drop 1 |> String.join " "

                                newElement =
                                    Element input.left input.top elementText 0 (max fontSize 4)

                                newModel =
                                    { model
                                        | elements = newElement :: model.elements
                                        , input = Nothing
                                    }
                            in
                            ( newModel, Cmd.none )

        Input query ->
            ( { model | input = model.input |> Maybe.map (\input -> { input | query = query }) }, Cmd.none )

        DoubleClick left top ->
            ( { model | input = Just { left = left, top = top, query = "", isLoading = False } }, Cmd.none )

        Drag element left top ->
            let
                lostElementList =
                    case model.beingDragged of
                        Nothing ->
                            []

                        Just ( lostElement, _, _ ) ->
                            [ lostElement ]
            in
            ( { model
                | beingDragged = Just ( element, left, top )
                , elements = model.elements ++ lostElementList |> List.filter ((/=) element)
              }
            , Cmd.none
            )

        DragEnd ->
            case model.beingDragged of
                Nothing ->
                    ( model, Cmd.none )

                Just ( lostElement, _, _ ) ->
                    ( { model | elements = lostElement :: model.elements, beingDragged = Nothing }, Cmd.none )

        DragOver ->
            ( model, Cmd.none )

        Drop left top ->
            case model.beingDragged of
                Nothing ->
                    ( model, Cmd.none )

                Just ( element, startLeft, startTop ) ->
                    let
                        repositionedElement =
                            { element
                                | left = element.left + (left - startLeft)
                                , top = element.top + (top - startTop)
                                , zIndex = nextZIndex model.elements
                            }
                    in
                    ( { model | beingDragged = Nothing, elements = repositionedElement :: model.elements }, Cmd.none )


viewElement : Element -> Html Msg
viewElement element =
    let
        { left, top, imageSource, zIndex, fontSize } =
            element

        commonAttributes =
            [ style "position" "absolute"
            , style "left" <| String.fromInt left ++ "px"
            , style "top" <| String.fromInt top ++ "px"
            , style "z-index" <| String.fromInt zIndex
            , style "border-radius" "15px"
            , draggable "true"
            , onDragStart <| Drag element
            , onDragEnd DragEnd
            ]
    in
    if fontSize > 0 then
        let
            attributes =
                [ style "font-size" <| String.fromInt fontSize ++ "px"
                , style "background-color" "lightgray"
                , style "padding" "10px"
                , style "font-weight" "bold"
                ]
                    ++ commonAttributes
        in
        div attributes [ text imageSource ]

    else
        img ([ src imageSource, width 240 ] ++ commonAttributes) []


view : Model -> Html Msg
view model =
    let
        maybeInput =
            case model.input of
                Nothing ->
                    []

                Just { left, top, query, isLoading } ->
                    [ form [ Events.onSubmit Submit ]
                        [ input
                            [ style "position" "absolute"
                            , style "left" <| String.fromInt left ++ "px"
                            , style "top" <| String.fromInt top ++ "px"
                            , disabled isLoading
                            , Events.onInput Input
                            , value query
                            ]
                            []
                        ]
                    ]

        importExportButtons =
            [ button [ Events.onClick Export ] [ text "export" ]
            , button [ Events.onClick Import ] [ text "import" ]
            ]
    in
    div
        [ style "position" "absolute"
        , style "width" "50000px"
        , style "height" "50000px"
        , style "background-color" "green"
        , onDoubleClick DoubleClick
        , onDragOver DragOver
        , onDrop Drop
        ]
        (importExportButtons ++ maybeInput ++ List.map viewElement model.elements)


onDoubleClick message =
    Events.on "dblclick" <| Decode.map2 message (Decode.field "clientX" Decode.int) (Decode.field "clientY" Decode.int)


onDragStart msg =
    Events.on "dragstart" <|
        Decode.map2 msg (Decode.field "clientX" Decode.int) (Decode.field "clientY" Decode.int)


onDragEnd msg =
    Events.on "dragend" <|
        Decode.succeed msg


onDragOver msg =
    Events.preventDefaultOn "dragover" <|
        Decode.succeed ( msg, True )


onDrop message =
    Events.preventDefaultOn "drop" <|
        Decode.map (\msg -> ( msg, True )) <|
            Decode.map2 message (Decode.field "clientX" Decode.int) (Decode.field "clientY" Decode.int)
