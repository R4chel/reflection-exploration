module Main exposing (..)

import Angle
import Browser
import Circle2d exposing (Circle2d)
import Direction2d exposing (Direction2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)
import Vector2d exposing (Vector2d)



-- CONSTANTS


size : Int
size =
    1000



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Object


type TopLeftCoordinates
    = TopLeftCoordinates


type alias Object =
    { position : Point2d Pixels TopLeftCoordinates
    , lightRay : Direction2d TopLeftCoordinates
    }


type alias Mirror =
    LineSegment2d Pixels TopLeftCoordinates



-- MODEL


type alias Model =
    { objects : List Object
    , mirrors : List Mirror
    }



-- INIT


init : Model
init =
    { objects =
        [ { position = Point2d.pixels 50 70
          , lightRay = Direction2d.degrees 20
          }
        ]
    , mirrors =
        [ LineSegment2d.from (Point2d.pixels 10 0) (Point2d.pixels 10 (Basics.toFloat size))
        ]
    }



-- VIEW


view : Model -> Html Msg
view model =
    let
        imageSize =
            String.fromInt size
    in
    Svg.svg
        [ width imageSize
        , height imageSize
        , viewBox (String.join "  " [ "0", "0", imageSize, imageSize ])
        ]
        (List.map viewMirror model.mirrors
            ++ List.map viewObject model.objects
        )


viewMirror : Mirror -> Svg msg
viewMirror mirror =
    Svg.lineSegment2d
        [ Attributes.stroke "grey"
        , Attributes.strokeWidth "5"
        ]
        mirror


lightPath : Object -> List Mirror -> Polyline2d Pixels TopLeftCoordinates
lightPath object mirrors =
    let
        initialLine =
            LineSegment2d.fromPointAndVector
                object.position
                (Direction2d.toVector object.lightRay
                    |> Vector2d.scaleTo (Basics.toFloat size |> pixels)
                )
    in
    let
        segments =
            [ initialLine ]
    in
    (object.position
        :: List.map LineSegment2d.endPoint segments
    )
        |> Polyline2d.fromVertices


viewLightPath : List Mirror -> Object -> Svg msg
viewLightPath mirrors object =
    let
        path =
            lightPath object mirrors
    in
    Svg.polyline2d
        [ Attributes.stroke "yellow"
        , Attributes.fill "none"
        , Attributes.strokeWidth "5"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        ]
        path


viewObject : Object -> Svg msg
viewObject object =
    Svg.circle2d
        [ Attributes.fill "blue"
        ]
        (Circle2d.withRadius (pixels 10)
            object.position
        )



-- UPDATE


type Msg
    = Noop


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model
