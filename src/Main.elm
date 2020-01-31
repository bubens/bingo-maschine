port module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Joker
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import List.Extra as List
import Random
import Random.Extra
import Random.List
import Random.Set
import Set exposing (Set)


type Hole
    = Hole



-- TYPES


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


type Input
    = Empty
    | Valid String
    | Invalid String


type Selection
    = BingoType TypeOfBingo
    | Size Int
    | Minimum String
    | Maximum String
    | Ordered Bool
    | Joker Bool
    | NumberOfCards String


type Msg
    = Noop
    | Selected Selection
    | StringsEntered String
    | CardGenerated Card
    | SubmitSettings


type TypeOfBingo
    = Strings
    | Numbers


type alias Colors =
    { text : Element.Color
    , darkText : Element.Color
    , color : Element.Color
    , brightBackground : Element.Color
    , darkBackground : Element.Color
    }


type alias Directions =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


type alias Card =
    List (List String)



-- MODEL


type alias Model =
    { title : String
    , size : Int
    , rangeMinimum : Int
    , rangeMaximum : Int
    , strings : Set String
    , typeOfBingo : TypeOfBingo
    , rawStringInput : String
    , rawMinimumInput : Input
    , rawMaximumInput : Input
    , ordered : Bool
    , joker : Bool
    , sampleCard : Maybe Card
    , numberOfCards : Int
    , rawNumberOfCardsInput : Input
    , createdCards : Maybe (List Card)
    }


type OutgoingMessageType
    = SaveState Model
    | CreateCards Model



-- INIT


fallbackModel : Model
fallbackModel =
    { typeOfBingo = Numbers
    , size = 5
    , rangeMinimum = 0
    , rangeMaximum = 100
    , strings = Set.empty
    , title = "BingoMaschine 4.0 - Fallback"
    , rawStringInput = ""
    , rawMinimumInput = Valid "0"
    , rawMaximumInput = Valid "100"
    , ordered = True
    , joker = False
    , sampleCard = Nothing
    , numberOfCards = 1
    , rawNumberOfCardsInput = Valid "1"
    , createdCards = Nothing
    }


modelDecoder : Decoder Model
modelDecoder =
    let
        toTypeOfBingo string =
            if string == "Numbers" then
                Numbers

            else
                Strings
    in
    Decode.succeed Model
        |> Decode.required "title" Decode.string
        |> Decode.required "size" Decode.int
        |> Decode.required "rangeMinimum" Decode.int
        |> Decode.required "rangeMaximum" Decode.int
        |> Decode.custom
            (Decode.field "strings" (Decode.list Decode.string)
                |> Decode.map Set.fromList
            )
        |> Decode.custom
            (Decode.field "typeOfBingo" Decode.string
                |> Decode.map toTypeOfBingo
            )
        |> Decode.required "rawStringInput" Decode.string
        |> Decode.custom
            (Decode.field "rawMinimumInput" Decode.string
                |> Decode.map stringToInput
            )
        |> Decode.custom
            (Decode.field "rawMaximumInput" Decode.string
                |> Decode.map stringToInput
            )
        |> Decode.required "ordered" Decode.bool
        |> Decode.required "joker" Decode.bool
        |> Decode.hardcoded Nothing
        |> Decode.required "numberOfCards" Decode.int
        |> Decode.custom
            (Decode.field "rawNumberOfCardsInput" Decode.string
                |> Decode.map stringToInput
            )
        |> Decode.hardcoded Nothing


init : Decode.Value -> ( Model, Cmd Msg )
init modelFromStorage =
    let
        initModel =
            modelFromStorage
                |> Decode.decodeValue modelDecoder
                |> Debug.log "modelFromStorage"
                |> Result.withDefault fallbackModel

        generator =
            cardGenerator initModel
    in
    ( initModel, Random.generate CardGenerated generator )



-- UTILS


colors : Colors
colors =
    { text = rgb255 244 243 241
    , darkText = rgb255 236 231 228
    , color = rgb255 223 216 211
    , brightBackground = rgb255 9 122 108
    , darkBackground = rgb255 0 66 86
    }


stringToInput : String -> Input
stringToInput string =
    if string == "" then
        Empty

    else
        let
            maybeInt =
                String.toInt string
        in
        case maybeInt of
            Just _ ->
                Valid string

            Nothing ->
                Invalid string


directions : Directions
directions =
    Directions 0 0 0 0


leftOf : b -> a -> ( a, b )
leftOf right left =
    ( left, right )


withCommand : Cmd msg -> Model -> ( Model, Cmd msg )
withCommand =
    leftOf



-- UPDATE


updateSettings : Selection -> Model -> Model
updateSettings selection model =
    case selection of
        Size selectedSize ->
            { model | size = selectedSize }

        BingoType selectedType ->
            { model | typeOfBingo = selectedType }

        Minimum stringMinimum ->
            let
                input =
                    stringToInput stringMinimum
            in
            case input of
                Empty ->
                    { model
                        | rawMinimumInput = Empty
                        , rangeMinimum = 0
                    }

                Valid x ->
                    { model
                        | rawMinimumInput = input
                        , rangeMinimum =
                            String.toInt x
                                |> Maybe.withDefault 0
                    }

                Invalid _ ->
                    { model
                        | rawMinimumInput = input
                        , rangeMinimum = 0
                    }

        Maximum stringMaximum ->
            let
                input =
                    stringToInput stringMaximum
            in
            case input of
                Empty ->
                    { model
                        | rawMaximumInput = Empty
                        , rangeMaximum = 100
                    }

                Valid x ->
                    { model
                        | rawMaximumInput = input
                        , rangeMaximum =
                            String.toInt x
                                |> Maybe.withDefault 100
                    }

                Invalid _ ->
                    { model
                        | rawMaximumInput = input
                        , rangeMaximum = 100
                    }

        Ordered isOrdered ->
            { model | ordered = isOrdered }

        Joker isWithJoker ->
            { model | joker = isWithJoker }

        NumberOfCards n ->
            let
                input =
                    stringToInput n
            in
            case input of
                Empty ->
                    { model
                        | rawNumberOfCardsInput = Empty
                        , numberOfCards = 0
                    }

                Valid x ->
                    { model
                        | rawNumberOfCardsInput = input
                        , numberOfCards =
                            String.toInt x
                                |> Maybe.withDefault 1
                    }

                Invalid _ ->
                    { model
                        | rawNumberOfCardsInput = input
                        , numberOfCards = 1
                    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Selected selection ->
            let
                nextModel =
                    updateSettings selection model
            in
            ( nextModel
            , Cmd.batch
                [ generateSampleCard nextModel
                , sendToOutbox (SaveState nextModel)
                ]
            )

        CardGenerated card ->
            { model | sampleCard = Just card }
                |> withCommand Cmd.none

        StringsEntered input ->
            let
                nextStrings =
                    String.split ";" input
                        |> List.map String.trim
                        |> Set.fromList

                newModel =
                    { model
                        | strings = nextStrings
                        , rawStringInput = input
                    }
            in
            ( newModel, generateSampleCard newModel )

        SubmitSettings ->
            ( model, sendToOutbox (CreateCards model) )



-- GENERATORS


generateOrdered : Model -> Random.Generator Card
generateOrdered { size, rangeMinimum, rangeMaximum } =
    let
        split =
            (rangeMaximum - rangeMinimum) // size
    in
    List.range 0 (size - 1)
        |> List.map ((+) rangeMinimum << (*) split)
        |> Random.Extra.traverse
            (\v ->
                Random.Set.set
                    size
                    (Random.int v (v + split))
            )
        |> Random.map
            (List.map (List.map String.fromInt << Set.toList))


generateFrom : Int -> List String -> Random.Generator Card
generateFrom size strings =
    case strings of
        [] ->
            Random.list
                (size ^ 2)
                (Random.constant " ")
                |> Random.map (List.groupsOf size)

        [ x ] ->
            Random.list
                (size ^ 2)
                (Random.constant x)
                |> Random.map (List.groupsOf size)

        x :: xs ->
            Random.Set.set
                (size ^ 2)
                (Random.uniform x xs)
                |> Random.map Set.toList
                |> Random.andThen Random.List.shuffle
                |> Random.map (List.groupsOf size)


cardGenerator : Model -> Random.Generator Card
cardGenerator model =
    case model.typeOfBingo of
        Strings ->
            generateFrom
                model.size
                (Set.toList model.strings)

        Numbers ->
            if model.ordered then
                generateOrdered model

            else
                generateFrom
                    model.size
                    (List.range model.rangeMinimum model.rangeMaximum
                        |> List.map String.fromInt
                    )


generateSampleCard : Model -> Cmd Msg
generateSampleCard =
    Random.generate CardGenerated << cardGenerator



-- VIEW


asDocument : String -> Element msg -> Document msg
asDocument title body =
    Document
        title
        [ Element.layout
            [ Background.color colors.darkBackground
            , Font.color colors.text
            ]
            (column
                [ height <| px 800
                , width <| px 800
                , centerX
                , centerY
                , spacing 20
                ]
                [ body ]
            )
        ]


viewHeader : String -> Element msg
viewHeader title =
    row
        [ Font.size 28
        , Font.color colors.color
        , width <| fill
        , paddingEach
            { directions
                | top = 20
                , bottom = 20
            }
        ]
        [ el
            [ centerX
            , width <| fill
            , Border.widthEach
                { directions
                    | bottom = 2
                }
            ]
            (text title)
        ]


viewInfobox : Element msg
viewInfobox =
    row
        [ Font.size 12
        , Font.color colors.color
        , width fill
        , Font.justify
        , padding 0
        ]
        [ paragraph []
            [ text "Erstelle jetzt deine top-starke Bingo-Zettel im Browser. Jetzt neu im praktischen PDF-Format."
            ]
        ]


viewSettingsLabel : String -> Input.Label Msg
viewSettingsLabel label =
    Input.labelAbove
        [ Font.bold
        , Font.size 18
        , spacing 10
        , paddingXY 0 5
        ]
        (text label)


viewTypeSelection : TypeOfBingo -> Element Msg
viewTypeSelection typeOfBingo =
    Input.radioRow
        [ spacing 10 ]
        { onChange = Selected << BingoType
        , selected = Just typeOfBingo
        , label =
            viewSettingsLabel "Bingo-Art wählen:"
        , options =
            [ Input.option Numbers (el [ Font.alignLeft ] (text "Klassisch"))
            , Input.option Strings (text "Eigene Eingaben")
            ]
        }


viewSelectSize : Model -> Element Msg
viewSelectSize model =
    let
        track =
            el
                [ width fill
                , height (px 2)
                , centerY
                , Background.color
                    colors.brightBackground
                , Border.rounded 2
                ]
                none

        thumb =
            Input.defaultThumb
    in
    column
        [ width fill ]
        [ Input.slider
            [ behindContent track ]
            { onChange =
                Selected << Size << round
            , label =
                viewSettingsLabel "Größe wählen:"
            , min = 3
            , max = 10
            , thumb = thumb
            , step = Just 1
            , value = toFloat model.size
            }
        , paragraph []
            [ el
                [ Font.bold
                , centerY
                ]
                (text "Reihen/Spalten: ")
            , el
                [ Font.family
                    [ Font.monospace
                    ]
                , Font.size 20
                ]
                (text <| String.fromInt model.size)
            ]
        ]


viewStringSettings : String -> Element Msg
viewStringSettings rawInput =
    Input.multiline
        [ Font.color colors.darkBackground
        , height <| px 200
        , width fill
        ]
        { onChange = StringsEntered
        , text = rawInput
        , placeholder =
            Just <|
                Input.placeholder
                    []
                    (text "Eigene Eingaben; durch Strichpunkt getrennt; so...")
        , label = viewSettingsLabel "Eigene Eingaben:"
        , spellcheck = False
        }


viewInput : String -> (String -> Selection) -> Input -> Element Msg
viewInput labelText selection input =
    let
        ( valueText, glow ) =
            case input of
                Empty ->
                    ( "", 0 )

                Valid string ->
                    ( string, 0 )

                Invalid string ->
                    ( string, 5 )
    in
    Input.text
        [ Border.glow (rgb255 255 0 0) glow
        , Font.color colors.darkBackground
        , Font.size 25
        , height <| px 30
        , padding 0
        , spacing 0
        , centerX
        , Font.family [ Font.monospace ]
        ]
        { onChange = Selected << selection
        , text = valueText
        , placeholder = Nothing
        , label =
            Input.labelLeft
                [ Font.size 14
                , centerY
                ]
                (text (labelText ++ ":"))
        }


viewNumberSettings : Model -> Element Msg
viewNumberSettings model =
    let
        header =
            el
                [ Font.size 18
                , Font.bold
                , paddingXY 0 5
                ]
                (text "Einstellungen Zahlen:")

        selectOrdered =
            Input.radioRow
                [ centerY
                , width fill
                , spacingXY 20 0
                ]
                { onChange = Selected << Ordered
                , selected = Just model.ordered
                , label =
                    Input.labelLeft
                        [ Font.family [ Font.monospace ]
                        , Font.size 14
                        , centerY
                        ]
                        (text "Geordnet:")
                , options =
                    [ Input.option
                        True
                        (el
                            [ Font.size 18
                            , Font.color colors.darkText
                            ]
                            (text "Ja")
                        )
                    , Input.option
                        False
                        (el
                            [ Font.size 18
                            , Font.color colors.darkText
                            ]
                            (text "Nein")
                        )
                    ]
                }
    in
    column
        [ width fill
        , spacing 5
        , height <| px 200
        ]
        [ header
        , viewInput "Minimum" Minimum model.rawMinimumInput
        , viewInput "Maximum" Maximum model.rawMaximumInput
        , selectOrdered
        ]


viewSelectJoker : Model -> Element Msg
viewSelectJoker model =
    if modBy 2 model.size == 1 then
        Input.radioRow
            [ centerY
            , width fill
            , spacingXY 20 0
            ]
            { onChange = Selected << Joker
            , selected = Just model.joker
            , label =
                Input.labelLeft
                    [ Font.family [ Font.monospace ]
                    , Font.size 14
                    , centerY
                    ]
                    (text "Jokerfeld:")
            , options =
                [ Input.option
                    True
                    (el
                        [ Font.size 18
                        , Font.color colors.darkText
                        ]
                        (text "Ja")
                    )
                , Input.option
                    False
                    (el
                        [ Font.size 18
                        , Font.color colors.darkText
                        ]
                        (text "Nein")
                    )
                ]
            }

    else
        none


viewSelectNumberOfCards : Model -> Element Msg
viewSelectNumberOfCards model =
    viewInput "Number of Cards" NumberOfCards model.rawNumberOfCardsInput


viewSettings : Model -> Element Msg
viewSettings model =
    let
        settings =
            case model.typeOfBingo of
                Strings ->
                    viewStringSettings model.rawStringInput

                Numbers ->
                    viewNumberSettings model
    in
    column
        [ Font.size 12
        , Font.color colors.color
        , width fill
        , height fill
        , spacing 20
        ]
        [ viewSelectSize model
        , viewTypeSelection model.typeOfBingo
        , settings
        , viewSelectJoker model
        , viewSelectNumberOfCards model
        ]


viewSampleCard : Model -> Element msg
viewSampleCard model =
    case model.sampleCard of
        Just card ->
            column
                [ width fill ]
                [ el
                    [ Font.size 24
                    , padding 5
                    , centerX
                    ]
                    (text "Beispiel-Zettel")
                , row
                    [ width fill
                    , Border.width 3
                    , Border.color colors.brightBackground
                    ]
                    (List.indexedMap
                        (\outerIndex col ->
                            List.indexedMap
                                (\innerIndex x ->
                                    let
                                        halfSize =
                                            toFloat (model.size - 1)
                                                / 2
                                                |> round

                                        value =
                                            if
                                                not model.joker
                                                    || (modBy 2 model.size == 0)
                                                    || not (outerIndex == halfSize)
                                                    || not (innerIndex == halfSize)
                                            then
                                                el
                                                    [ width shrink
                                                    , height shrink
                                                    , centerX
                                                    , centerY
                                                    , Font.family [ Font.monospace ]
                                                    ]
                                                    (text x)

                                            else
                                                el
                                                    [ width shrink
                                                    , height shrink
                                                    , centerX
                                                    , centerY
                                                    ]
                                                    (html <| Joker.render 50 50)
                                    in
                                    el
                                        [ height <| px 60
                                        , width fill
                                        , Border.width 1
                                        , Border.color colors.brightBackground
                                        ]
                                        value
                                )
                                col
                                |> (::)
                                    (el
                                        [ height <| fillPortion 2
                                        , Font.bold
                                        ]
                                        (text "B")
                                    )
                                |> column
                                    [ width <| fillPortion 1 ]
                        )
                        card
                    )
                ]

        Nothing ->
            el [] (text "Here won't be Card")


viewSubmit : Element Msg
viewSubmit =
    row
        [ Font.size 12
        , Font.color colors.color
        , width fill
        , height fill
        , spacing 20
        ]
        [ Input.button
            []
            { onPress = Just SubmitSettings
            , label =
                text "Karten erzeugen"
            }
        ]


view : Model -> Document Msg
view model =
    let
        header =
            viewHeader model.title
    in
    column
        []
        [ header
        , row
            [ spacing 20 ]
            [ column
                [ width <| px 200
                , height fill
                , spacing 20
                ]
                [ viewInfobox
                , viewSettings model
                , viewSubmit
                ]
            , column
                [ width <| px 600
                , height fill
                ]
                [ viewSampleCard model ]
            ]
        ]
        |> asDocument model.title



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- PORTS


modelEncoder : Model -> Encode.Value
modelEncoder model =
    let
        typeToString bingoType =
            case bingoType of
                Numbers ->
                    "Numbers"

                Strings ->
                    "Strings"

        inputToString input =
            case input of
                Valid str ->
                    str

                Invalid str ->
                    str

                Empty ->
                    ""
    in
    Encode.object
        [ ( "title", Encode.string model.title )
        , ( "size", Encode.int model.size )
        , ( "rangeMinimum", Encode.int model.rangeMinimum )
        , ( "rangeMaximum", Encode.int model.rangeMaximum )
        , ( "strings", Encode.list Encode.string <| Set.toList model.strings )
        , ( "typeOfBingo", Encode.string <| typeToString model.typeOfBingo )
        , ( "rawStringInput", Encode.string model.rawStringInput )
        , ( "rawMinimumInput", Encode.string <| inputToString model.rawMinimumInput )
        , ( "rawMaximumInput", Encode.string <| inputToString model.rawMaximumInput )
        , ( "ordered", Encode.bool model.ordered )
        , ( "joker", Encode.bool model.joker )
        , ( "sampleCard", Encode.null )
        , ( "numberOfCards", Encode.int model.numberOfCards )
        , ( "rawNumberOfCardsInput", Encode.string <| inputToString model.rawNumberOfCardsInput )
        , ( "createdCards", Encode.null )
        ]


sendToOutbox : OutgoingMessageType -> Cmd msg
sendToOutbox outMsgType =
    case outMsgType of
        SaveState model ->
            Encode.object
                [ ( "model", modelEncoder model )
                , ( "type", Encode.string "SaveState" )
                ]
                |> outbox

        CreateCards model ->
            Encode.object
                [ ( "model", modelEncoder model )
                , ( "type", Encode.string "CreateCards" )
                ]
                |> outbox


port outbox : Encode.Value -> Cmd msg



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
