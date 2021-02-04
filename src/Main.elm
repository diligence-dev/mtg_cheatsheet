module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Debug
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Attribute, Html, button, div, form, img, input, text)
import Html.Attributes exposing (disabled, draggable, height, id, src, style, value, width)
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import MessageToast exposing (MessageToast)
import Task


--log =
--    Debug.log


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


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
    , messageToast : MessageToast Msg
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- MessageToast provides a subscription to close automatically which is easy to use.
    MessageToast.subscriptions model.messageToast


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
            , messageToast = MessageToast.init UpdatedSimpleMessageToast
            }
    in
    ( model, Cmd.none )


type Msg
    = DoubleClick (Maybe Element) Int Int
    | Drag Element Int Int
    | DragEnd
    | DragOver
    | Drop (Maybe String) Int Int
    | Input String
    | InputLostFocus
    | Submit
    | Response Int Int (Result Http.Error String)
    | Export
    | Import
    | FileLoaded File
    | FileContentLoaded String
    | NoOp
    | UpdatedSimpleMessageToast (MessageToast Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedSimpleMessageToast updatedMessageToast ->
            ( { model | messageToast = updatedMessageToast }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        FileContentLoaded fileContent ->
            case Decode.decodeString (Decode.list elementDecoder) fileContent of
                Err _ ->
                    let
                        newMessageToast =
                            model.messageToast |> MessageToast.warning |> MessageToast.withMessage "could not load file"
                    in
                    ( { model | messageToast = newMessageToast }, Cmd.none )

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
                    let
                        newMessageToast =
                            model.messageToast |> MessageToast.warning |> MessageToast.withMessage "card not found"

                        newModel =
                            { model
                                | input = model.input |> Maybe.map (\input -> { input | isLoading = False })
                                , messageToast = newMessageToast
                            }
                    in
                    ( newModel, Cmd.none )

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

        InputLostFocus ->
            ( { model | input = Nothing }, Cmd.none )

        Input query ->
            ( { model | input = model.input |> Maybe.map (\input -> { input | query = query }) }, Cmd.none )

        DoubleClick maybeElement left top ->
            let
                newModel =
                    case maybeElement of
                        Nothing ->
                            { model | input = Just { left = left, top = top, query = "", isLoading = False } }

                        Just element ->
                            let
                                newQuery =
                                    if element.fontSize > 0 then
                                        String.fromInt element.fontSize ++ " " ++ element.imageSource

                                    else
                                        ""
                            in
                            { model
                                | input = Just { left = left, top = top, query = newQuery, isLoading = False }
                                , elements = model.elements |> List.filter ((/=) element)
                            }
            in
            ( newModel, Task.attempt (\_ -> NoOp) (Dom.focus "activeInput") )

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

        Drop link left top ->
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
            , onDoubleClick <| DoubleClick (Just element)
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
                            , id "activeInput"
                            , Events.onBlur InputLostFocus
                            ]
                            []
                        ]
                    ]

        importExportButtons =
            [ div [ style "position" "fixed" ]
                [ button [ Events.onClick Export ] [ text "export" ]
                , button [ Events.onClick Import ] [ text "import" ]
                ]
            ]

        messageToast =
            [ MessageToast.view model.messageToast ]
    in
    div
        [ style "position" "absolute"
        , style "width" "50000px"
        , style "height" "50000px"
        , style "background-color" "green"
        , onDoubleClick <| DoubleClick Nothing
        , onDragOver DragOver
        , onDrop <| Drop Nothing
        ]
        (messageToast ++ importExportButtons ++ maybeInput ++ List.map viewElement model.elements)


onDoubleClick message =
    Events.stopPropagationOn "dblclick" <|
        Decode.map (\msg -> ( msg, True )) <|
            Decode.map2 message (Decode.field "clientX" Decode.int) (Decode.field "clientY" Decode.int)


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
