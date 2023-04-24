module Main exposing (..)

import Axis2d exposing (Axis2d)
import Browser
import Browser.Events
import Circle2d
import Direction2d exposing (Direction2d)
import Draggable
import Geometry.Svg as Svg
import Html exposing (Html, button, div, text)
import Json.Decode as D
import LineSegment2d exposing (LineSegment2d, endPoint, startPoint)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)
import Svg.Events as Events exposing (..)
import Vector2d



-- CONSTANTS


size : Int
size =
    1000



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Object


type alias Id =
    Int


type TopLeftCoordinates
    = TopLeftCoordinates


type alias Object =
    { id : Id
    , position : Point2d Pixels TopLeftCoordinates
    , lightRay : Direction2d TopLeftCoordinates
    }



-- Mirror


type alias Mirror =
    { id : Id
    , position : LineSegment2d Pixels TopLeftCoordinates
    }


type alias MousePosition = Point2d Pixels TopLeftCoordinates



-- CURSOR MODE: what does pressing the cursor do


type CursorMode
    = AddObject
    | ChooseLightRay MousePosition



-- MODEL


type alias Model =
    { objects : List
    Object
    , mirrors : List Mirror
    , nextId : Int
    , cursorMode : Maybe CursorMode
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { objects =
    [
    { id = 1
    , position = Point2d.pixels 50 70
    , lightRay = Direction2d.degrees 50
    }
    ]
    , mirrors =
            [ { id = 2, position = LineSegment2d.from (Point2d.pixels 200 0) (Point2d.pixels 200 (Basics.toFloat size)) }
            , { id = 3, position = LineSegment2d.from (Point2d.pixels 0 400) (Point2d.pixels 400 400) }
            ]
      , nextId = 4
      , cursorMode = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        imageSize =
            String.fromInt size
    in
    div []
        [ button [ onClick AddObjectButtonPressed ] [ text "Add Object" ]
        , Svg.svg
            [ width imageSize
            , height imageSize
            , viewBox (String.join "  " [ "0", "0", imageSize, imageSize ])
            ]
            ( ( List.concatMap (viewObject model) model.objects )

             
                ++ List.map
                    viewMirror
                    model.mirrors
            )
        ]


viewMirror : Mirror -> Svg msg
viewMirror mirror =
    Svg.lineSegment2d
        [ Attributes.stroke "grey"
        , Attributes.strokeWidth "5"
        ]
        mirror.position


mirrorAsAxis : Mirror -> Axis2d Pixels TopLeftCoordinates
mirrorAsAxis mirror =
    Axis2d.throughPoints (startPoint mirror.position) (endPoint mirror.position) |> Maybe.withDefault (Axis2d.through (startPoint mirror.position) (Direction2d.degrees -90))


chooseMirror : List Mirror -> LineSegment2d Pixels TopLeftCoordinates -> Maybe ( Mirror, Point2d Pixels TopLeftCoordinates )
chooseMirror mirrors lightPath =
    List.filterMap (\mirror -> LineSegment2d.intersectionPoint lightPath mirror.position |> Maybe.map (Tuple.pair mirror)) mirrors
        -- filter out the mirror(s) that contain the startPoint by removing points where the intersection point is the startPoint
        |> List.filter (\mirror_intersectionPoint -> Tuple.second mirror_intersectionPoint |> Point2d.equalWithin (Pixels.float 1) (startPoint lightPath) |> not)
        |> Quantity.minimumBy
            (\mirror_point -> Tuple.second mirror_point |> Point2d.distanceFrom (startPoint lightPath))



-- TODO findLightPath does not yet handling potentially infinite loop


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


viewObject : Model -> Object -> List(  Svg Msg )
viewObject model object =
           let shape = 
                      Svg.circle2d
                          [ Attributes.fill "blue"
                          , Events.onMouseOver Noop
                          ]
                          (Circle2d.withRadius (pixels 10)
                              object.position
                          )
        in
        [ viewLightPath model.mirrors object, shape]



-- UPDATE


type Msg
    = Noop
    | AddObjectButtonPressed
    | MouseClicked MousePosition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            let
                () =
                    Debug.log "noop" ()
            in
            ( model
            , Cmd.none
            )

        AddObjectButtonPressed ->
            ( { model | cursorMode = Just AddObject }
            , Cmd.none
            )

        MouseClicked position ->
            ( mouseClicked model position, Cmd.none )


mouseClicked : Model -> MousePosition -> Model
mouseClicked model position =
    case model.cursorMode of
        Nothing ->
            model

        Just AddObject ->
            { model | cursorMode = Just (ChooseLightRay position) }
        Just (ChooseLightRay lastPosition) ->
             case Direction2d.from lastPosition position of
                  Nothing ->
             -- TODO give helpful error messages
                          model
                  Just lightRay ->
                       let newObject = {id = model.nextId , position = lastPosition, lightRay = lightRay} in
                       { model | nextId = model.nextId + 1, cursorMode = Nothing, objects = newObject :: model.objects}



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onClick
            (D.map MouseClicked
                (D.map2 Point2d.pixels
                    (D.field "pageX" D.float)
                    (D.field "pageY" D.float)
                )
            )
        ]
