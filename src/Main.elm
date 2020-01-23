module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
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


type Msg
    = Noop
    | Selected Selection
    | StringsEntered String
    | CardGenerated Card


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
    }



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


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        initModel =
            flags
                |> Decode.decodeValue modelDecoder
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
            Just x ->
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Selected selection ->
            let
                generateSampleCard =
                    Random.generate CardGenerated << cardGenerator

                newModel =
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
            in
            ( newModel, generateSampleCard newModel )

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
            ( newModel, Random.generate CardGenerated (cardGenerator newModel) )



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



--|> Random.andMap Random.shuffle


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
                , Background.color colors.brightBackground
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

        ( minimumText, minimumGlow ) =
            case model.rawMinimumInput of
                Empty ->
                    ( "", 0 )

                Valid string ->
                    ( string, 0 )

                Invalid string ->
                    ( string, 5 )

        selectMinimum =
            Input.text
                [ Border.glow (rgb255 255 0 0) minimumGlow
                , Font.color colors.darkBackground
                , Font.size 25
                , height <| px 30
                , padding 0
                , spacing 0
                , centerX
                , Font.family [ Font.monospace ]
                ]
                { onChange = Selected << Minimum
                , text = minimumText
                , placeholder = Nothing
                , label =
                    Input.labelLeft
                        [ Font.size 14
                        , centerY
                        ]
                        (text "Minimum: ")
                }

        ( maximumText, maximumGlow ) =
            case model.rawMaximumInput of
                Empty ->
                    ( "", 0 )

                Valid string ->
                    ( string, 0 )

                Invalid string ->
                    ( string, 5 )

        selectMaximum =
            Input.text
                [ Border.glow (rgb255 255 0 0) maximumGlow
                , Font.color colors.darkBackground
                , Font.size 25
                , height <| px 30
                , padding 0
                , spacing 0
                , centerX
                , Font.family [ Font.monospace ]
                ]
                { onChange = Selected << Maximum
                , text = maximumText
                , placeholder = Nothing
                , label =
                    Input.labelLeft
                        [ Font.size 14
                        , centerY
                        ]
                        (text "Maximum: ")
                }

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
        , selectMinimum
        , selectMaximum
        , selectOrdered
        ]


viewSettings : Model -> Element Msg
viewSettings model =
    let
        selectJoker =
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
        , case model.typeOfBingo of
            Strings ->
                viewStringSettings model.rawStringInput

            Numbers ->
                viewNumberSettings model
        , selectJoker
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
                    (List.map
                        (\col ->
                            List.map
                                (\x ->
                                    el
                                        [ height <| px 40
                                        , width fill
                                        , Border.width 1
                                        , Border.color colors.brightBackground
                                        ]
                                        (el
                                            [ width shrink
                                            , height shrink
                                            , centerX
                                            , centerY
                                            , Font.family [ Font.monospace ]
                                            ]
                                            (text x)
                                        )
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
subscriptions model =
    Sub.none



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
