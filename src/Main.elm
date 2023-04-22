module Main exposing (..)

import Axis2d exposing (Axis2d)
import Browser
import Circle2d
import Direction2d exposing (Direction2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d exposing (LineSegment2d, endPoint, startPoint)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)
import Vector2d



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



-- Mirror


type alias Mirror =
    LineSegment2d Pixels TopLeftCoordinates



-- MODEL


type alias Model =
    { object : Object
    , mirrors : List Mirror
    }



-- INIT


init : Model
init =
    { object =
        { position = Point2d.pixels 50 70
        , lightRay = Direction2d.degrees 20
        }
    , mirrors =
        [ LineSegment2d.from (Point2d.pixels 200 0) (Point2d.pixels 200 (Basics.toFloat size))
        , LineSegment2d.from (Point2d.pixels 0 400) (Point2d.pixels 400 400)
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
        ([ viewLightPath model.mirrors model.object
         , viewObject model.object
         ]
            ++ List.map
                viewMirror
                model.mirrors
        )


viewMirror : Mirror -> Svg msg
viewMirror mirror =
    Svg.lineSegment2d
        [ Attributes.stroke "grey"
        , Attributes.strokeWidth "5"
        ]
        mirror


lightPathOneMirror : Object -> Mirror -> Polyline2d Pixels TopLeftCoordinates
lightPathOneMirror object mirror =
    let
        lightSegment =
            LineSegment2d.fromPointAndVector
                object.position
                (Direction2d.toVector object.lightRay
                    |> Vector2d.scaleTo (Basics.toFloat size |> pixels)
                )
    in
    let
        segments =
            case LineSegment2d.intersectionPoint lightSegment mirror of
                Nothing ->
                    [ lightSegment ]

                Just p ->
                    let
                        segment1 =
                            LineSegment2d.from (startPoint lightSegment) p
                    in
                    let
                        segment2 =
                            LineSegment2d.from p (endPoint lightSegment) |> LineSegment2d.mirrorAcross (mirrorAsAxis mirror)
                    in
                    [ segment1, segment2 ]
    in
    (object.position
        :: List.map LineSegment2d.endPoint segments
    )
        |> Polyline2d.fromVertices


mirrorAsAxis : Mirror -> Axis2d Pixels TopLeftCoordinates
mirrorAsAxis mirror =
    Axis2d.throughPoints (startPoint mirror) (endPoint mirror) |> Maybe.withDefault (Axis2d.through (startPoint mirror) (Direction2d.degrees -90))


chooseMirror : List Mirror -> LineSegment2d Pixels TopLeftCoordinates -> Maybe ( Mirror, Point2d Pixels TopLeftCoordinates )
chooseMirror mirrors lightPath =
    List.filterMap (\mirror -> LineSegment2d.intersectionPoint lightPath mirror |> Maybe.map (Tuple.pair mirror)) mirrors
        -- filter out the mirror(s) that contain the startPoint by removing points where the intersection point is the startPoint
        |> List.filter (\mirror_intersectionPoint -> Tuple.second mirror_intersectionPoint |> Point2d.equalWithin (Pixels.float 1) (startPoint lightPath) |> not)
        |> Quantity.minimumBy
            (\mirror_point -> Tuple.second mirror_point |> Point2d.distanceFrom (startPoint lightPath))



-- Not yet handling potentially infinite loop


findLightPath : List Mirror -> LineSegment2d Pixels TopLeftCoordinates -> List (Point2d Pixels TopLeftCoordinates)
findLightPath mirrors path =
    case chooseMirror mirrors path of
        Nothing ->
            [ startPoint path, endPoint path ]

        Just ( mirror, intersectionPoint ) ->
            let
                newSegment =
                    LineSegment2d.from intersectionPoint (endPoint path) |> LineSegment2d.mirrorAcross (mirrorAsAxis mirror)
            in
            startPoint path :: findLightPath mirrors newSegment


viewLightPath : List Mirror -> Object -> Svg msg
viewLightPath mirrors object =
    let
        lightSegment =
            LineSegment2d.fromPointAndVector
                object.position
                (Direction2d.toVector object.lightRay
                    |> Vector2d.scaleTo (Basics.toFloat size |> pixels)
                )
    in
    let
        path =
            findLightPath mirrors lightSegment
                |> Polyline2d.fromVertices
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
