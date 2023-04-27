module Main exposing (..)

import Angle
import Axis2d exposing (Axis2d)
import Browser
import Circle2d
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Draggable
import Draggable.Events
import Element exposing (alignLeft, centerX, column, el, padding, rgb255, row, text)
import Element.Background as Background
import Element.Input as Input
import Element.Region as Region
import Geometry.Svg as Svg
import Html exposing (Html)
import Json.Decode as D
import LineSegment2d exposing (LineSegment2d, endPoint, startPoint)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polyline2d
import Quantity
import Random
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)
import Svg.Events
import Vector2d exposing (Vector2d)



-- CONSTANTS


imageSize : String
imageSize =
    "1000"


roomSize : Float
roomSize =
    1000


lightLength : Float
lightLength =
    500



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Room


type alias Room =
    Rectangle2d Pixels TopLeftCoordinates



-- Object


type alias Id =
    Int


type ObjectComponent
    = ObjectPosition
    | LightRay


type MirrorComponent
    = StartPoint
    | Segment
    | EndPoint


type SelectableComponentId
    = ObjectSelected ObjectComponent Id
    | MirrorSelected MirrorComponent Id


type TopLeftCoordinates
    = TopLeftCoordinates


type alias Object =
    { id : Id
    , position : Point2d Pixels TopLeftCoordinates
    , lightRay : Direction2d TopLeftCoordinates
    }


generateObject : Room -> Id -> Random.Generator Object
generateObject room id =
    Random.map3
        Object
        (Random.constant id)
        (Rectangle2d.randomPoint room)
        Direction2d.random



-- Mirror


type alias Mirror =
    { id : Id
    , position : LineSegment2d Pixels TopLeftCoordinates
    }


type alias MousePosition =
    Point2d Pixels TopLeftCoordinates



-- MODEL


type alias Model =
    { room : Room
    , objects : Dict Id Object
    , mirrors : Dict Id Mirror
    , nextId : Int
    , drag : Draggable.State SelectableComponentId
    , currentlyDragging : Maybe SelectableComponentId
    , lastMousePosition : MousePosition
    , highlightedElement : Maybe Id
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { room =
            Rectangle2d.from (Point2d.pixels 0 0) (Point2d.pixels roomSize roomSize)
      , objects =
            Dict.fromList
                [ ( 1
                  , { id = 1
                    , position = Point2d.pixels 50 70
                    , lightRay = Direction2d.degrees 50
                    }
                  )
                ]
      , mirrors =
            Dict.fromList
                [ ( 2
                  , { id = 2
                    , position =
                        LineSegment2d.from
                            (Point2d.pixels 200 0)
                            (Point2d.pixels 200 500)
                    }
                  )
                , ( 3
                  , { id = 3
                    , position =
                        LineSegment2d.from
                            (Point2d.pixels 0 400)
                            (Point2d.pixels 400 400)
                    }
                  )
                ]
      , nextId = 4
      , drag = Draggable.init
      , currentlyDragging = Nothing
      , lastMousePosition = Point2d.origin
      , highlightedElement = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    column [ padding 30 ]
        [ el [ Region.heading 1, centerX, padding 50 ] (text "Reflection Exploration")
        , row [ alignLeft, padding 5 ]
            [ Element.html (viewScene model)
            , Element.row [ padding 20 ]
                [ Element.column [ padding 20, Element.spacing 20 ]
                    [ Input.button [ Background.color (rgb255 41 152 252), padding 20 ]
                        { onPress = Just ResetButtonPressed
                        , label = text "Reset"
                        }
                    , Input.button
                        [ Background.color (rgb255 41 152 252), padding 20 ]
                        { onPress = Just AddObjectButtonPressed
                        , label = text "Add Another Object"
                        }
                    ]
                ]
            ]
        ]
        |> Element.layout []


viewScene : Model -> Html Msg
viewScene model =
    Svg.svg
        [ width imageSize
        , height imageSize
        , viewBox (String.join "  " [ "0", "0", imageSize, imageSize ])
        ]
        (List.concat
            [ [ viewRoom model.room ]
            , List.map (viewObject model) (Dict.values model.objects)
            , List.map
                (viewMirror model)
                (Dict.values model.mirrors)
            ]
        )


viewRoom : Room -> Svg msg
viewRoom room =
    Svg.rectangle2d
        [ Attributes.stroke "black"
        , Attributes.fill "burlywood"
        ]
        room


mouseOverEvents : Id -> List (Svg.Attribute Msg)
mouseOverEvents id =
    [ Svg.Events.onMouseOver (MouseOver (Just id))
    , Svg.Events.onMouseOut (MouseOver Nothing)
    ]


viewMirror : Model -> Mirror -> Svg Msg
viewMirror model mirror =
    let
        isHighlighted : Bool
        isHighlighted =
            model.highlightedElement == Just mirror.id
    in
    Svg.g
        (List.concat
            [ [ fill "drakgrey"
              , stroke "grey"
              ]
            , if isHighlighted then
                [ stroke "green"
                , strokeWidth "10"
                , fill "blue"
                ]

              else
                [ strokeWidth "10" ]
            , mouseOverEvents mirror.id
            ]
        )
        [ Svg.lineSegment2d
            [ Draggable.mouseTrigger (MirrorSelected Segment mirror.id) DragMsg
            ]
            mirror.position
        , Svg.circle2d
            [ Draggable.mouseTrigger (MirrorSelected StartPoint mirror.id) DragMsg
            ]
            (Circle2d.withRadius (pixels 10) (startPoint mirror.position))
        , Svg.circle2d
            [ Draggable.mouseTrigger (MirrorSelected EndPoint mirror.id) DragMsg
            ]
            (Circle2d.withRadius (pixels 10) (endPoint mirror.position))
        ]


mirrorAsAxis : Mirror -> Axis2d Pixels TopLeftCoordinates
mirrorAsAxis mirror =
    Axis2d.throughPoints (startPoint mirror.position) (endPoint mirror.position)
        -- Maybe.withDefault is used because if the mirror has lenght 0,
        -- the axis can't be determined. This is extremely unlikely,
        -- and possibly doesn't need to be worried about so in that case an arbitrary direction is chosen.
        |> Maybe.withDefault (Axis2d.through (startPoint mirror.position) (Direction2d.degrees -90))


findClosestMirror : List Mirror -> LineSegment2d Pixels TopLeftCoordinates -> Maybe ( Mirror, Point2d Pixels TopLeftCoordinates )
findClosestMirror mirrors lightPath =
    mirrors
        |> List.filterMap
            (\mirror ->
                LineSegment2d.intersectionPoint lightPath mirror.position
                    |> Maybe.map (Tuple.pair mirror)
            )
        -- filter out the mirror(s) that contain the startPoint by removing points where the intersection point is the startPoint.
        -- This is was added to solve the problem of after an intersection the closest mirror being the mirror that had just been touched because it is distance 0.
        -- This does create a new problem of not handling the case of multiple mirrors meeting at one point correctly becuase additional mirrors are also filtered out.
        -- TODO case of light hitting intersection of mirrors is not handled correctly
        |> List.filter
            (\mirror_intersectionPoint ->
                Tuple.second mirror_intersectionPoint
                    |> Point2d.equalWithin (Pixels.float 1) (startPoint lightPath)
                    |> not
            )
        |> Quantity.minimumBy
            (\mirror_point -> Tuple.second mirror_point |> Point2d.distanceFrom (startPoint lightPath))


findLightPath : List Mirror -> LineSegment2d Pixels TopLeftCoordinates -> List (Point2d Pixels TopLeftCoordinates)
findLightPath mirrors path =
    case findClosestMirror mirrors path of
        Nothing ->
            [ startPoint path, endPoint path ]

        Just ( mirror, intersectionPoint ) ->
            let
                newSegment =
                    LineSegment2d.from intersectionPoint (endPoint path)
                        |> LineSegment2d.mirrorAcross (mirrorAsAxis mirror)
            in
            startPoint path :: findLightPath mirrors newSegment


viewLightPath : List Mirror -> Object -> Bool -> Svg Msg
viewLightPath mirrors object highlight =
    let
        lightSegment =
            object.lightRay
                |> Direction2d.toVector
                |> Vector2d.scaleTo (pixels lightLength)
                |> LineSegment2d.fromPointAndVector
                    object.position

        path =
            findLightPath mirrors lightSegment
                |> Polyline2d.fromVertices
    in
    Svg.polyline2d
        [ Attributes.stroke
            (if highlight then
                "yellow"

             else
                "#FFFEB8"
            )
        , Attributes.strokeWidth
            (if highlight then
                "8"

             else
                "5"
            )
        , Attributes.fill "none"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        , Draggable.customMouseTrigger (ObjectSelected LightRay object.id)
            mousePositionDecoder
            DragLightRay
        ]
        path


viewObject : Model -> Object -> Svg Msg
viewObject model object =
    let
        isHighlighted : Bool
        isHighlighted =
            model.highlightedElement == Just object.id
    in
    let
        radius =
            if isHighlighted then
                pixels 30

            else
                pixels 25
    in
    let
        shape =
            Svg.circle2d
                [ Attributes.fill "blue"
                , Attributes.stroke
                    (if isHighlighted then
                        "green"

                     else
                        "none"
                    )
                , Draggable.mouseTrigger
                    (ObjectSelected ObjectPosition object.id)
                    DragMsg
                ]
                (Circle2d.withRadius radius
                    object.position
                )
    in
    Svg.g (mouseOverEvents object.id)
        [ viewLightPath (Dict.values model.mirrors) object isHighlighted, shape ]



-- UPDATE


type Msg
    = ResetButtonPressed
    | MouseOver (Maybe Id)
    | OnDragBy (Vector2d Pixels TopLeftCoordinates)
    | DragMsg (Draggable.Msg SelectableComponentId)
    | DragLightRay (Draggable.Msg SelectableComponentId) MousePosition
    | StartDragging SelectableComponentId
    | StopDragging
    | AddObjectButtonPressed
    | AddObject Object


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetButtonPressed ->
            init ()

        AddObjectButtonPressed ->
            let
                objectId =
                    model.nextId
            in
            ( { model | nextId = model.nextId + 1 }, Random.generate AddObject (generateObject model.room objectId) )

        AddObject object ->
            ( { model | objects = Dict.insert object.id object model.objects }
            , Cmd.none
            )

        MouseOver id ->
            ( { model | highlightedElement = id }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        DragLightRay dragMsg position ->
            { model | lastMousePosition = position }
                |> Draggable.update dragConfig dragMsg

        StartDragging id ->
            ( { model | currentlyDragging = Just id }, Cmd.none )

        StopDragging ->
            ( { model | currentlyDragging = Nothing }, Cmd.none )

        OnDragBy delta ->
            ( onDragBy model delta, Cmd.none )


dragConfig : Draggable.Config SelectableComponentId Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy (\( dx, dy ) -> Vector2d.pixels dx dy |> OnDragBy)
        , Draggable.Events.onDragStart StartDragging
        , Draggable.Events.onDragEnd StopDragging
        ]


onDragBy : Model -> Vector2d Pixels TopLeftCoordinates -> Model
onDragBy model delta =
    case model.currentlyDragging of
        Nothing ->
            -- Note: I think getting an onDragMsg if nothing is being dragged may represent a bug, and potentially something should be logged or this should be handled in some way
            model

        Just (ObjectSelected ObjectPosition id) ->
            -- not having an {object/mirror} corresponding to a dragged id is very suprising. Currently that case is being silently ignored
            { model
                | objects =
                    Dict.update id
                        (Maybe.map (dragObject model.lastMousePosition delta ObjectPosition))
                        model.objects
            }

        Just (ObjectSelected LightRay id) ->
            let
                lastMousePosition =
                    model.lastMousePosition
                        |> Point2d.translateBy delta
            in
            { model
                | lastMousePosition = lastMousePosition
                , objects =
                    Dict.update id
                        (Maybe.map (dragObject lastMousePosition delta LightRay))
                        model.objects
            }

        Just (MirrorSelected component id) ->
            { model
                | mirrors =
                    Dict.update id
                        (Maybe.map (dragMirror delta component))
                        model.mirrors
            }


dragObject : MousePosition -> Vector2d Pixels TopLeftCoordinates -> ObjectComponent -> Object -> Object
dragObject mousePosition delta component object =
    case component of
        ObjectPosition ->
            { object
                | position =
                    object.position
                        |> Point2d.translateBy delta
            }

        LightRay ->
            case Direction2d.from object.position mousePosition of
                Nothing ->
                    -- if the mouse position is exactly on the object there is no direction. Current logic is to ignore this case an not update the light ray
                    object

                Just directionToMouse ->
                    { object | lightRay = directionToMouse }


dragMirror : Vector2d Pixels TopLeftCoordinates -> MirrorComponent -> Mirror -> Mirror
dragMirror delta component mirror =
    let
        position =
            case component of
                Segment ->
                    LineSegment2d.translateBy delta mirror.position

                StartPoint ->
                    startPoint mirror.position
                        |> Point2d.translateBy delta
                        |> (\start -> LineSegment2d.from start (endPoint mirror.position))

                EndPoint ->
                    endPoint mirror.position
                        |> Point2d.translateBy delta
                        |> LineSegment2d.from (startPoint mirror.position)
    in
    { mirror | position = position }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Draggable.subscriptions DragMsg model.drag


mousePositionDecoder : D.Decoder MousePosition
mousePositionDecoder =
    D.map2 Point2d.pixels
        (D.field "offsetX" D.float)
        (D.field "offsetY" D.float)
