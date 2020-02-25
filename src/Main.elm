port module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element)
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
import Result.Extra
import Set exposing (Set)



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
    | WithJoker Bool
    | NumberOfCards String


type Msg
    = SettingSelected Selection
    | ValuesEntered String
    | SampleCardGenerated (Result String Card)
    | CardsGenerated (Result String (List Card))
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


type CellValue
    = ValueField String
    | JokerField


type alias Card =
    List (List CellValue)



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
    , sampleCard : Result String Card
    , numberOfCards : Int
    , rawNumberOfCardsInput : Input
    , errorMessage : Maybe String
    }


type OutgoingMessageType
    = SaveState Model
    | CreateCards (List Card)
    | SendJoker String



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
    , sampleCard = Err "So sample card generated"
    , numberOfCards = 1
    , rawNumberOfCardsInput = Valid "1"
    , errorMessage = Nothing
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
        |> Decode.hardcoded (Err "No sample card generated")
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
                |> Result.withDefault fallbackModel

        generator =
            generateCard initModel
    in
    initModel
        |> withCommand
            (Cmd.batch
                [ Random.generate SampleCardGenerated generator
                , sendToOutbox (SendJoker <| Joker.asString 50 50)
                ]
            )



-- UTILS


colors : Colors
colors =
    { text = Element.rgb255 244 243 241
    , darkText = Element.rgb255 236 231 228
    , color = Element.rgb255 223 216 211
    , brightBackground = Element.rgb255 9 122 108
    , darkBackground = Element.rgb255 0 66 86
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

        WithJoker isWithJoker ->
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
        SettingSelected selection ->
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

        SampleCardGenerated card ->
            { model
                | sampleCard = card
                , errorMessage = Nothing
            }
                |> withCommand Cmd.none

        ValuesEntered input ->
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
            ( model, generateCards model )

        CardsGenerated cardResult ->
            case cardResult of
                Ok cards ->
                    { model | errorMessage = Nothing }
                        |> withCommand (sendToOutbox <| CreateCards cards)

                Err errString ->
                    { model | errorMessage = Just errString }
                        |> withCommand Cmd.none



-- GENERATORS


toCard : Bool -> Int -> List (List String) -> Card
toCard hasJoker size card =
    let
        jokerIndex =
            size // 2

        isEven =
            modBy 2 size == 0
    in
    List.indexedMap
        (\colIndex column ->
            List.indexedMap
                (\cellIndex cell ->
                    if not hasJoker || isEven then
                        ValueField cell

                    else if not (cellIndex == jokerIndex) then
                        ValueField cell

                    else if not (colIndex == jokerIndex) then
                        ValueField cell

                    else
                        JokerField
                )
                column
        )
        card


generateOrdered : Model -> Random.Generator (Result String Card)
generateOrdered { joker, size, rangeMinimum, rangeMaximum } =
    let
        rangeLen =
            1 + rangeMaximum - rangeMinimum

        rangeLongEnough =
            rangeLen >= size * size

        valuesPerColumn =
            rangeLen // size
    in
    Random.Extra.result
        (Random.constant rangeLongEnough)
        (Random.constant "Zahlenbereich nicht groß genug um Bingokarten zu erzeugen.")
        (List.range rangeMinimum rangeMaximum
            |> List.map String.fromInt
            |> List.groupsOf valuesPerColumn
            |> List.map
                (Random.List.shuffle
                    >> Random.map
                        -- The result of Random.List.shuffle appears to be more random
                        -- at the end of the list. It's probably related to this:
                        -- https://github.com/elm-community/random-extra/issues/24
                        -- That's why the list is reversed.
                        (List.take size << List.reverse)
                )
            |> Random.Extra.combine
            |> Random.map (toCard joker size)
        )


generateCardFromValues : Model -> List String -> Random.Generator (Result String Card)
generateCardFromValues { size, joker } values =
    let
        enoughValues =
            List.length values >= size * size
    in
    Random.Extra.result
        (Random.constant enoughValues)
        (Random.constant "Nicht genügend Werte um Karte zu erzeugen.")
        (Random.List.shuffle values
            |> Random.map
                (List.take (size * size)
                    >> List.groupsOf size
                    >> toCard joker size
                )
        )


generateCard : Model -> Random.Generator (Result String Card)
generateCard model =
    case model.typeOfBingo of
        Strings ->
            generateCardFromValues
                model
                (Set.toList model.strings)

        Numbers ->
            if model.ordered then
                generateOrdered model

            else
                generateCardFromValues
                    model
                    (List.range model.rangeMinimum model.rangeMaximum
                        |> List.map String.fromInt
                    )


generateSampleCard : Model -> Cmd Msg
generateSampleCard =
    Random.generate SampleCardGenerated << generateCard


generateCards : Model -> Cmd Msg
generateCards model =
    let
        listOfCards =
            Random.list
                model.numberOfCards
                (generateCard model)
                |> Random.map Result.Extra.combine
    in
    Random.generate CardsGenerated listOfCards



-- VIEW


asDocument : String -> Element msg -> Document msg
asDocument title body =
    Document
        title
        [ Element.layout
            [ Background.color colors.darkBackground
            , Font.color colors.text
            ]
            (Element.column
                [ Element.height <| Element.px 800
                , Element.width <| Element.px 800
                , Element.centerX
                , Element.centerY
                , Element.spacing 20
                ]
                [ body ]
            )
        ]


viewHeader : String -> Element msg
viewHeader title =
    Element.row
        [ Font.size 28
        , Font.color colors.color
        , Element.width <| Element.fill
        , Element.paddingEach
            { directions
                | top = 20
                , bottom = 20
            }
        ]
        [ Element.el
            [ Element.centerX
            , Element.width <| Element.fill
            , Border.widthEach
                { directions
                    | bottom = 2
                }
            ]
            (Element.text title)
        ]


viewInfobox : Element msg
viewInfobox =
    Element.row
        [ Font.size 12
        , Font.color colors.color
        , Element.width Element.fill
        , Font.justify
        , Element.padding 0
        ]
        [ Element.paragraph []
            [ Element.text "Erstelle jetzt deine top-starke Bingo-Zettel im Browser. Jetzt neu im praktischen PDF-Format."
            ]
        ]


viewSettingsLabel : String -> Input.Label Msg
viewSettingsLabel label =
    Input.labelAbove
        [ Font.bold
        , Font.size 18
        , Element.spacing 10
        , Element.paddingXY 0 5
        ]
        (Element.text label)


viewTypeSelection : TypeOfBingo -> Element Msg
viewTypeSelection typeOfBingo =
    Input.radioRow
        [ Element.spacing 10 ]
        { onChange = SettingSelected << BingoType
        , selected = Just typeOfBingo
        , label =
            viewSettingsLabel "Bingo-Art wählen:"
        , options =
            [ Input.option Numbers (Element.el [ Font.alignLeft ] (Element.text "Klassisch"))
            , Input.option Strings (Element.text "Eigene Eingaben")
            ]
        }


viewSelectSize : Model -> Element Msg
viewSelectSize model =
    let
        track =
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color
                    colors.brightBackground
                , Border.rounded 2
                ]
                Element.none

        thumb =
            Input.defaultThumb
    in
    Element.column
        [ Element.width Element.fill ]
        [ Input.slider
            [ Element.behindContent track ]
            { onChange =
                SettingSelected << Size << round
            , label =
                viewSettingsLabel "Größe wählen:"
            , min = 3
            , max = 10
            , thumb = thumb
            , step = Just 1
            , value = toFloat model.size
            }
        , Element.paragraph []
            [ Element.el
                [ Font.bold
                , Element.centerY
                ]
                (Element.text "Reihen/Spalten: ")
            , Element.el
                [ Font.family
                    [ Font.monospace
                    ]
                , Font.size 20
                ]
                (Element.text <| String.fromInt model.size)
            ]
        ]


viewStringSettings : String -> Element Msg
viewStringSettings rawInput =
    Input.multiline
        [ Font.color colors.darkBackground
        , Element.height <| Element.px 200
        , Element.width Element.fill
        ]
        { onChange = ValuesEntered
        , text = rawInput
        , placeholder =
            Just <|
                Input.placeholder
                    []
                    (Element.text "Eigene Eingaben; durch Strichpunkt getrennt; so...")
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
        [ Border.glow (Element.rgb255 255 0 0) glow
        , Font.color colors.darkBackground
        , Font.size 25
        , Element.height <| Element.px 30
        , Element.padding 0
        , Element.spacing 0
        , Element.centerX
        , Font.family [ Font.monospace ]
        ]
        { onChange = SettingSelected << selection
        , text = valueText
        , placeholder = Nothing
        , label =
            Input.labelLeft
                [ Font.size 14
                , Element.centerY
                ]
                (Element.text (labelText ++ ":"))
        }


viewNumberSettings : Model -> Element Msg
viewNumberSettings model =
    let
        header =
            Element.el
                [ Font.size 18
                , Font.bold
                , Element.paddingXY 0 5
                ]
                (Element.text "Einstellungen Zahlen:")

        selectOrdered =
            Input.radioRow
                [ Element.centerY
                , Element.width Element.fill
                , Element.spacingXY 20 0
                ]
                { onChange = SettingSelected << Ordered
                , selected = Just model.ordered
                , label =
                    Input.labelLeft
                        [ Font.family [ Font.monospace ]
                        , Font.size 14
                        , Element.centerY
                        ]
                        (Element.text "Geordnet:")
                , options =
                    [ Input.option
                        True
                        (Element.el
                            [ Font.size 18
                            , Font.color colors.darkText
                            ]
                            (Element.text "Ja")
                        )
                    , Input.option
                        False
                        (Element.el
                            [ Font.size 18
                            , Font.color colors.darkText
                            ]
                            (Element.text "Nein")
                        )
                    ]
                }
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 5
        , Element.height <| Element.px 200
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
            [ Element.centerY
            , Element.width Element.fill
            , Element.spacingXY 20 0
            ]
            { onChange = SettingSelected << WithJoker
            , selected = Just model.joker
            , label =
                Input.labelLeft
                    [ Font.family [ Font.monospace ]
                    , Font.size 14
                    , Element.centerY
                    ]
                    (Element.text "Jokerfeld:")
            , options =
                [ Input.option
                    True
                    (Element.el
                        [ Font.size 18
                        , Font.color colors.darkText
                        ]
                        (Element.text "Ja")
                    )
                , Input.option
                    False
                    (Element.el
                        [ Font.size 18
                        , Font.color colors.darkText
                        ]
                        (Element.text "Nein")
                    )
                ]
            }

    else
        Element.none


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
    Element.column
        [ Font.size 12
        , Font.color colors.color
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 20
        ]
        [ viewSelectSize model
        , viewTypeSelection model.typeOfBingo
        , settings
        , viewSelectJoker model
        , viewSelectNumberOfCards model
        ]


viewSampleCard : Card -> Element msg
viewSampleCard card =
    List.map
        (\col ->
            List.map
                (\cell ->
                    let
                        value =
                            case cell of
                                ValueField str ->
                                    Element.el
                                        [ Element.width Element.shrink
                                        , Element.height Element.shrink
                                        , Element.centerX
                                        , Element.centerY
                                        , Font.family [ Font.monospace ]
                                        ]
                                        (Element.text str)

                                JokerField ->
                                    Element.el
                                        [ Element.width Element.shrink
                                        , Element.height Element.shrink
                                        , Element.centerX
                                        , Element.centerY
                                        ]
                                        (Element.html <| Joker.render 50 50)
                    in
                    Element.el
                        [ Element.height <| Element.px 60
                        , Element.width Element.fill
                        , Border.width 1
                        , Border.color colors.brightBackground
                        ]
                        value
                )
                col
                |> Element.column
                    [ Element.width <| Element.fillPortion 1 ]
        )
        card
        |> Element.row
            [ Element.width Element.fill
            , Border.width 3
            , Border.color colors.brightBackground
            ]


viewResult : Model -> Element msg
viewResult model =
    case model.sampleCard of
        Ok card ->
            Element.column
                [ Element.width Element.fill ]
                [ Element.el
                    [ Font.size 24
                    , Element.padding 5
                    , Element.centerX
                    ]
                    (Element.text "Beispiel-Zettel")
                , viewSampleCard card
                ]

        Err errString ->
            Element.column
                [ Element.width Element.fill
                , Element.centerX
                , Element.centerY
                , Border.solid
                , Border.width 2
                , Border.color colors.text
                , Element.padding 5
                , Element.spacing 5
                ]
                [ Element.el
                    [ Font.size 28
                    , Element.centerX
                    ]
                    (Element.text "Fehler:")
                , Element.el
                    [ Font.size 20
                    , Element.centerX
                    ]
                    (Element.text errString)
                ]


viewSubmit : Maybe String -> Element Msg
viewSubmit errorMessage =
    let
        error =
            case errorMessage of
                Just msg ->
                    Element.el
                        [ Font.color <| Element.rgb255 255 0 0 ]
                        (Element.text msg)

                Nothing ->
                    Element.none
    in
    Element.row
        [ Font.size 12
        , Font.color colors.color
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 20
        ]
        [ Input.button
            []
            { onPress = Just SubmitSettings
            , label =
                Element.text "Karten erzeugen"
            }
        , error
        ]


view : Model -> Document Msg
view model =
    let
        header =
            viewHeader model.title
    in
    Element.column
        []
        [ header
        , Element.row
            [ Element.spacing 20 ]
            [ Element.column
                [ Element.width <| Element.px 200
                , Element.height Element.fill
                , Element.spacing 20
                ]
                [ viewInfobox
                , viewSettings model
                , viewSubmit model.errorMessage
                ]
            , Element.column
                [ Element.width <| Element.px 600
                , Element.height Element.fill
                ]
                [ viewResult model ]
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
        ]


cardsEncoder : List Card -> Encode.Value
cardsEncoder cards =
    Encode.list
        (Encode.list
            (Encode.list
                (\val ->
                    case val of
                        ValueField str ->
                            Encode.string str

                        JokerField ->
                            Encode.string "%_JOKER_%"
                )
            )
        )
        cards


sendToOutbox : OutgoingMessageType -> Cmd msg
sendToOutbox outMsgType =
    case outMsgType of
        SaveState model ->
            Encode.object
                [ ( "payload", modelEncoder model )
                , ( "type", Encode.string "SaveState" )
                ]
                |> outbox

        CreateCards cards ->
            Encode.object
                [ ( "payload", cardsEncoder cards )
                , ( "type", Encode.string "CreateCards" )
                ]
                |> outbox

        SendJoker imgData ->
            Encode.object
                [ ( "payload", Encode.string imgData )
                , ( "type", Encode.string "SendJoker" )
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
