module Joker exposing (asImageData, asString, render)

import Base64
import Html exposing (Html)
import Svg.String as Svg
import Svg.String.Attributes as Svg



{- Credits:
   Vectors based on Sogeking's Vectors (https://commons.wikimedia.org/wiki/User:Barbaking)
   originally lincensed under  CC-BY-SA 2.5
   converted to elm-code with levelteams.com's online tool (https://levelteams.com/svg-to-elm)
   and modified by me!
-}


create : Float -> Float -> Svg.Html msg
create w h =
    let
        viewBoxStr =
            "0 0 "
                ++ String.fromFloat w
                ++ " "
                ++ String.fromFloat h

        scaleStr =
            "scale("
                ++ String.fromFloat (w / 120)
                ++ ", "
                ++ String.fromFloat (h / 140)
                ++ ")"
    in
    Svg.svg
        [ Svg.height <| String.fromFloat h
        , Svg.width <| String.fromFloat w
        , Svg.viewBox viewBoxStr
        ]
        [ Svg.g
            [ Svg.transform scaleStr
            ]
            [ Svg.circle
                [ Svg.transform "translate(-26,-4)"
                , Svg.cy "31"
                , Svg.cx "31"
                , Svg.attribute "strokeMiterlimit" "4"
                , Svg.r "5"
                , Svg.fill "#000"
                ]
                []
            , Svg.circle
                [ Svg.transform "translate(84,-4)"
                , Svg.cy "31"
                , Svg.cx "31"
                , Svg.attribute "strokeMiterlimit" "4"
                , Svg.r "5"
                , Svg.fill "#000"
                ]
                []
            , Svg.circle
                [ Svg.transform "translate(28.8,-26)"
                , Svg.cy "31"
                , Svg.cx "31"
                , Svg.attribute "strokeMiterlimit" "4"
                , Svg.r "5"
                , Svg.fill "#000"
                ]
                []
            , Svg.node "path"
                [ Svg.fill "#000"
                , Svg.attribute "d" "m55.4,140c-1.2789-3.1348-2.6203-7.4102-8.2689-8.0241-8.2844-0.60755-15.807-1.2138-14.779-10.299,14.954-0.12045,17.978-14.728,14.064-14.585-7.3516,0.10612-3.6294,6.4722-15.138,7.5284-0.0705-2.206,0.208-4.7111-0.58873-5.8126-2.8027-3.0714-9.907-0.0483-11.23-4.2599,0.41469-4.7012,7.5184-14.325,8.9011-15.395-1.6108-3.7356-1.5929-16.971,1.9946-18.699h60.976c5.627,17.412-5.5093,26.451-5.9748,34.593,0.19343,4.1399,5.5154,9.1112,7.6699,11.024z"
                ]
                []
            , Svg.node "path"
                [ Svg.fill "#000"
                , Svg.attribute "d" "M59.8,11.37c-3.138,9.905-7.943,24.438-10.875,34.406-7.395-6.83-17.769-19.095-37.281-17.844,17.031,6.229,18.344,20.471,18.344,35.562h59.625c0-15.092,1.3132-29.334,18.344-35.562-19.515-1.252-29.89,11.014-37.284,17.844-2.932-9.968-7.737-24.502-10.875-34.406z"
                ]
                []
            ]
        ]


render : Float -> Float -> Html msg
render w h =
    create w h
        |> Svg.toHtml


asString : Float -> Float -> String
asString w h =
    create w h
        |> Svg.toString 0


asImageData : Float -> Float -> String
asImageData w h =
    asString w h
        |> Base64.encode
        |> (++) "data:image/svg+xml;base64,"
