module Main exposing (Model, Msg, main)

import Axis2d exposing (Axis2d)
import Browser
import Circle2d
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Draggable
import Draggable.Events
import Element exposing (Element, alignLeft, centerX, column, el, padding, rgb255, row, text)
import Element.Background as Background
import Element.Input as Input
import Element.Region as Region
import Geometry.Svg as Svg
import Html exposing (Html)
import Json.Decode as D
import LineSegment2d exposing (LineSegment2d, endPoint, startPoint)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
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
    1000



-- comparisonTolerance is used for determing if two floats are equal.
-- Note the specific value for comparisonTolerance was chosen relatively arbitrarily not tested and this choice may cause some surprising behaviors.


comparisonTolerance : Quantity Float Pixels
comparisonTolerance =
    pixels 1



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- TYPES


type Coordinates
    = Coordinates



-- Room


type alias Room =
    Rectangle2d Pixels Coordinates


type alias Id =
    Int



-- EyeComponent is selectable part of an eye


type EyeComponent
    = EyePosition
    | LightRay



-- MirrorComponent is the selectable part of a mirror


type MirrorComponent
    = StartPoint
    | Segment
    | EndPoint


type SelectableComponentId
    = EyeSelected EyeComponent Id
    | MirrorSelected MirrorComponent Id
    | ObjectSelected Id



-- Object


type alias Object =
    { position : Point2d Pixels Coordinates
    , radius : Quantity Float Pixels
    , id : Id
    }


generateObject : Room -> Random.Generator (Id -> Object)
generateObject room =
    -- Ids need to be unique across all elements so the id is not attached until the object is added to the model
    Random.map2
        Object
        (Rectangle2d.randomPoint room)
        (Random.map pixels (Random.float 15 30))



-- Eye


type alias Eye =
    { position : Point2d Pixels Coordinates
    , lightRay : Direction2d Coordinates
    , id : Id
    }


generateEye : Room -> Random.Generator (Id -> Eye)
generateEye room =
    -- Ids need to be unique across eyes and mirrors so the id is not attached until the eye is added to the odel
    Random.map2
        Eye
        (Rectangle2d.randomPoint room)
        Direction2d.random



-- Mirror


type alias Mirror =
    { position : LineSegment2d Pixels Coordinates
    , id : Id
    }


generateMirror : Room -> Random.Generator (Id -> Mirror)
generateMirror room =
    Random.map
        Mirror
        (Random.map2 LineSegment2d.from
            (Rectangle2d.randomPoint room)
            (Rectangle2d.randomPoint room)
        )



-- Mouse Position


type alias MousePosition =
    Point2d Pixels Coordinates



-- MODEL


type alias Model =
    { room : Room
    , objects : Dict Id Object
    , eyes : Dict Id Eye
    , mirrors : Dict Id Mirror
    , nextId : Int
    , drag : Draggable.State SelectableComponentId
    , currentlyDragging : Maybe SelectableComponentId
    , lastMousePosition : MousePosition
    , highlightedElement : Maybe Id
    }



-- INIT


emptyModel : Model
emptyModel =
    { room =
        Rectangle2d.from (Point2d.pixels 0 0) (Point2d.pixels roomSize roomSize)
    , objects = Dict.empty
    , eyes = Dict.empty
    , mirrors = Dict.empty
    , nextId = 1
    , drag = Draggable.init
    , currentlyDragging = Nothing
    , lastMousePosition = Point2d.origin
    , highlightedElement = Nothing
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( scenario Scenario1
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
                    [ styledButton (ScenarioButtonPressed Scenario1) "Scenario 1"
                    , styledButton (ScenarioButtonPressed Scenario2) "Scenario 2"
                    , styledButton (ScenarioButtonPressed Scenario3) "Scenario 3"
                    , styledButton (ScenarioButtonPressed Scenario4) "Scenario 4"
                    , styledButton (ScenarioButtonPressed Scenario5) "Scenario 5"
                    , styledButton AddObjectButtonPressed "Add Another Object"
                    , styledButton AddEyeButtonPressed "Add Another Eye"
                    , styledButton AddMirrorButtonPressed "Add Another Mirror"
                    , styledButton ClearSceneButtonPressed "Clear Scene"
                    ]
                ]
            , Element.textColumn [ Element.spacing 10, Element.padding 10, alignLeft ]
                [ text "Welcome! This is a tool designed to help explore how mirrors and eyes interact. Please explore."
                , el [] Element.none
                , el [] Element.none
                , Element.paragraph [] [ text "Here are some ways you can interact with it." ]
                , Element.paragraph [] [ Element.text "• Move the eyes by clicking and dragging an eye (blue circle )" ]
                , Element.paragraph [] [ Element.text "• Change the angle of light emitted from eye by clicking on light ray (yellow line) and moving the mouse. Currently all eyes emit light of the same brightness (represented by the length of the line) and there isn't a control to change that (yet)." ]
                , Element.paragraph [] [ Element.text "• Mirrors are grey lines. Move the whole mirror by selecting somewhere in the middle nad drag the cursor. You can also move the end points of the mirror (grey circles at the mirror ends)" ]
                , Element.paragraph [] [ Element.text "• Add in new mirrors and eyes by pressing the buttons that say 'Add Another Eye' or 'Add Another Mirror'. That will add in a mirror or eye in a random position, you can then move it around to put it where you want it." ]
                , el [] Element.none
                , Element.paragraph []
                    [ Element.text "Play around and have some fun. Here are some places to get started." ]
                , Element.paragraph []
                    [ Element.text "There are a few premade scenarios to explore." ]
                , Element.paragraph []
                    [ Element.text "Scenario 1 is one mirror and one eye. This is a great place to start. What happens when you move the eye? Does the angle change? What about when you change the angle of the light? Try moving the mirror. How does of moving the mirror without changing which way the mirror faces (by dragging in middle of the mirror instead of one of the end points) change what the light does?" ]
                , Element.paragraph []
                    [ Element.text "In scenario 2, what happens to the path of the light as you change one of mirrors by selecting an end point? Whats the difference between scenario 2 and scenario 3? Can you turn scenario 2 in scenario 3?" ]
                , Element.paragraph []
                    [ Element.text "In Scenario 4, without moving the eye and only changing it's angle of light, can you get the light out of the box with light touching exactly 2 mirrors? Exactly 3 mirrors? 4 mirrors? No mirrors? What if the light was longer? " ]
                , Element.paragraph [] [ Element.text "Try playing around with multiple eyes. (Add eyes with the button). When can the path of two eyes' light rays reach other? When can't they? (There isn't a preset scenario for this, so make your own)" ]
                ]
            ]
        ]
        |> Element.layout []


styledButton : Msg -> String -> Element Msg
styledButton action label =
    Input.button
        [ Background.color (rgb255 41 152 252), padding 20 ]
        { onPress = Just action
        , label = text label
        }


viewScene : Model -> Html Msg
viewScene model =
    let
        lightPaths : List (Polyline2d Pixels Coordinates)
        lightPaths =
            List.map (calculateLightPath (Dict.values model.mirrors)) (Dict.values model.eyes)
    in
    Svg.svg
        [ width imageSize
        , height imageSize
        , viewBox (String.join "  " [ "0", "0", imageSize, imageSize ])
        ]
        (List.concat
            [ [ viewRoom model.room ]
            , List.map (viewEye model) (Dict.values model.eyes)
            , List.map (viewObject model lightPaths) (Dict.values model.objects)
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


viewObject : Model -> List (Polyline2d Pixels Coordinates) -> Object -> Svg Msg
viewObject model lightPaths object =
    let
        isHighlighted : Bool
        isHighlighted =
            (model.highlightedElement == Just object.id)
                || (case model.currentlyDragging of
                        Just (ObjectSelected id) ->
                            id == object.id

                        _ ->
                            False
                   )

        isSeen : Bool
        isSeen =
            List.any (pathIntersectsObject object) lightPaths

        radius : Quantity Float Pixels
        radius =
            if isHighlighted then
                object.radius |> Quantity.multiplyBy 1.2

            else
                object.radius

        shape : Svg Msg
        shape =
            Svg.circle2d
                [ Attributes.fill
                    (if isSeen then
                        "red"

                     else if isHighlighted then
                        "aqua"

                     else
                        "teal"
                    )
                , Draggable.mouseTrigger
                    (ObjectSelected object.id)
                    DragMsg
                ]
                (Circle2d.withRadius radius
                    object.position
                )

        debuggingCollision : List (Svg Msg)
        debuggingCollision =
            List.concatMap Polyline2d.segments lightPaths
                |> List.filterMap (\segment -> Axis2d.throughPoints (startPoint segment) (endPoint segment))
                |> List.map
                    (\axis ->
                        Point2d.projectOnto axis object.position
                            |> Circle2d.withRadius (pixels 10)
                            |> Svg.circle2d [ Attributes.fill "purple" ]
                    )
    in
    Svg.g (mouseOverEvents object.id) ([ shape ] ++ debuggingCollision)


viewMirror : Model -> Mirror -> Svg Msg
viewMirror model mirror =
    let
        isHighlighted : Bool
        isHighlighted =
            model.highlightedElement
                == Just mirror.id
                || (case model.currentlyDragging of
                        Just (MirrorSelected _ id) ->
                            id == mirror.id

                        _ ->
                            False
                   )
    in
    Svg.g
        (List.concat
            [ if isHighlighted then
                [ stroke "green"
                , strokeWidth "15"
                ]

              else
                [ stroke "grey"
                , strokeWidth "12"
                ]
            , [ fill "lightgrey" ]
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


mirrorAsAxis : Mirror -> Axis2d Pixels Coordinates
mirrorAsAxis mirror =
    Axis2d.throughPoints (startPoint mirror.position) (endPoint mirror.position)
        -- Maybe.withDefault is used because if the mirror has lenght 0,
        -- the axis can't be determined. This is extremely unlikely,
        -- and possibly doesn't need to be worried about so in that case an arbitrary direction is chosen.
        |> Maybe.withDefault (Axis2d.through (startPoint mirror.position) (Direction2d.degrees -90))


findClosestMirror : List Mirror -> LineSegment2d Pixels Coordinates -> Maybe ( Mirror, Point2d Pixels Coordinates )
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
                    |> Point2d.equalWithin comparisonTolerance (startPoint lightPath)
                    |> not
            )
        |> Quantity.minimumBy
            (\mirror_point -> Tuple.second mirror_point |> Point2d.distanceFrom (startPoint lightPath))


findLightPath : List Mirror -> LineSegment2d Pixels Coordinates -> { lightPath : List (Point2d Pixels Coordinates), mirroredSegments : List (LineSegment2d Pixels Coordinates) }
findLightPath mirrors path =
    case findClosestMirror mirrors path of
        Nothing ->
            { lightPath = [ startPoint path, endPoint path ], mirroredSegments = [] }

        Just ( mirror, intersectionPoint ) ->
            let
                pathContinuation : LineSegment2d Pixels Coordinates
                pathContinuation =
                    LineSegment2d.from intersectionPoint (endPoint path)
                        |> LineSegment2d.mirrorAcross (mirrorAsAxis mirror)

                reflection : LineSegment2d Pixels Coordinates
                reflection =
                    LineSegment2d.from (startPoint path) intersectionPoint
                        |> LineSegment2d.mirrorAcross (mirrorAsAxis mirror)

                result =
                    findLightPath mirrors pathContinuation
            in
            { result
                | lightPath = startPoint path :: result.lightPath
                , mirroredSegments = reflection :: result.mirroredSegments
            }


pointIsOnSegment : LineSegment2d Pixels Coordinates -> Point2d Pixels Coordinates -> Bool
pointIsOnSegment segment point =
    let
        -- solve for tX in parameterized form segment
        x0 : Quantity Float Pixels
        x0 =
            Point2d.xCoordinate (startPoint segment)

        x1 : Quantity Float Pixels
        x1 =
            Point2d.xCoordinate (endPoint segment)

        tX : Float
        tX =
            Quantity.difference (Point2d.xCoordinate point) x0
                |> Quantity.ratio (Quantity.difference x1 x0)
    in
    0 <= tX && tX <= 1 && (LineSegment2d.interpolate segment tX |> Point2d.equalWithin comparisonTolerance point)


segmentIntersectsObject : Object -> LineSegment2d Pixels Coordinates -> Bool
segmentIntersectsObject object segment =
    case Axis2d.throughPoints (startPoint segment) (endPoint segment) of
        Nothing ->
            False

        Just axis ->
            let
                projectedPoint : Point2d Pixels Coordinates
                projectedPoint =
                    Point2d.projectOnto axis object.position

                px : Quantity Float Pixels
                px =
                    Point2d.xCoordinate projectedPoint

                x0 : Quantity Float Pixels
                x0 =
                    Point2d.xCoordinate (startPoint segment)

                x1 : Quantity Float Pixels
                x1 =
                    Point2d.xCoordinate (endPoint segment)
            in
            -- check projected point is within object that projected point is within endpoints of segment
            (projectedPoint |> Point2d.distanceFrom object.position |> Quantity.abs |> Quantity.lessThanOrEqualTo object.radius)
                && Quantity.greaterThanOrEqualTo (Quantity.min x0 x1) px
                && Quantity.lessThanOrEqualTo (Quantity.max x0 x1) px


pathIntersectsObject : Object -> Polyline2d Pixels Coordinates -> Bool
pathIntersectsObject object path =
    Polyline2d.segments path
        |> List.any (segmentIntersectsObject object)


calculateLightPath : List Mirror -> Eye -> Polyline2d Pixels Coordinates
calculateLightPath mirrors eye =
    let
        lightSegment : LineSegment2d Pixels Coordinates
        lightSegment =
            eye.lightRay
                |> Direction2d.toVector
                |> Vector2d.scaleTo (pixels lightLength)
                |> LineSegment2d.fromPointAndVector
                    eye.position

        { lightPath, mirroredSegments } =
            findLightPath mirrors lightSegment
    in
    lightPath |> Polyline2d.fromVertices


viewLightPath : List Mirror -> Eye -> Bool -> Svg Msg
viewLightPath mirrors eye highlight =
    let
        lightSegment : LineSegment2d Pixels Coordinates
        lightSegment =
            eye.lightRay
                |> Direction2d.toVector
                |> Vector2d.scaleTo (pixels lightLength)
                |> LineSegment2d.fromPointAndVector
                    eye.position

        { lightPath, mirroredSegments } =
            findLightPath mirrors lightSegment

        path : Polyline2d Pixels Coordinates
        path =
            lightPath |> Polyline2d.fromVertices

        lightPathSvg =
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
                , strokeOpacity "0.9"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Draggable.customMouseTrigger (EyeSelected LightRay eye.id)
                    mousePositionDecoder
                    DragLightRay
                ]
                path
    in
    Svg.g []
        (lightPathSvg
            :: List.map
                (Svg.lineSegment2d
                    [ Attributes.stroke "#FFFEB8"
                    , Attributes.strokeWidth "5"
                    , Attributes.fill "none"
                    , strokeOpacity "0.9"
                    , Attributes.strokeLinecap "round"
                    , Attributes.strokeLinejoin "round"
                    , Attributes.strokeDasharray "10,10"
                    ]
                )
                mirroredSegments
        )


viewEye : Model -> Eye -> Svg Msg
viewEye model eye =
    let
        isHighlighted : Bool
        isHighlighted =
            (model.highlightedElement == Just eye.id)
                || (case model.currentlyDragging of
                        Just (EyeSelected _ id) ->
                            id == eye.id

                        _ ->
                            False
                   )

        radius : Quantity Float Pixels
        radius =
            if isHighlighted then
                pixels 30

            else
                pixels 25

        eyeBall : Svg Msg
        eyeBall =
            Svg.circle2d
                [ Attributes.fill
                    (if isHighlighted then
                        "green"

                     else
                        "white"
                    )
                , Draggable.mouseTrigger
                    (EyeSelected EyePosition eye.id)
                    DragMsg
                ]
                (Circle2d.withRadius radius
                    eye.position
                )

        irisCenter : Point2d Pixels Coordinates
        irisCenter =
            eye.position
                |> Point2d.translateIn eye.lightRay (radius |> Quantity.multiplyBy 0.5)

        iris : Svg Msg
        iris =
            Svg.circle2d
                [ Attributes.fill "purple"
                ]
                (radius |> Quantity.multiplyBy 0.35 |> Circle2d.atPoint irisCenter)

        pupil : Svg Msg
        pupil =
            Svg.circle2d
                [ Attributes.fill "black"
                ]
                (radius |> Quantity.multiplyBy 0.22 |> Circle2d.atPoint irisCenter)
    in
    Svg.g (mouseOverEvents eye.id)
        [ viewLightPath (Dict.values model.mirrors) eye isHighlighted, eyeBall, iris, pupil ]



-- UPDATE


type Msg
    = ClearSceneButtonPressed
    | ScenarioButtonPressed WhichScenario
    | AddObjectButtonPressed
    | AddObject (Id -> Object)
    | AddEyeButtonPressed
    | AddEye (Id -> Eye)
    | AddMirrorButtonPressed
    | AddMirror (Id -> Mirror)
    | MouseOver (Maybe Id)
    | OnDragBy (Vector2d Pixels Coordinates)
    | DragMsg (Draggable.Msg SelectableComponentId)
    | DragLightRay (Draggable.Msg SelectableComponentId) MousePosition
    | StartDragging SelectableComponentId
    | StopDragging


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearSceneButtonPressed ->
            ( emptyModel, Cmd.none )

        ScenarioButtonPressed whichScenario ->
            ( scenario whichScenario, Cmd.none )

        AddObjectButtonPressed ->
            ( model, Random.generate AddObject (generateObject model.room) )

        AddObject objectWithoutId ->
            ( addObject objectWithoutId model
            , Cmd.none
            )

        AddEyeButtonPressed ->
            ( model, Random.generate AddEye (generateEye model.room) )

        AddEye eyeWithoutId ->
            ( addEye eyeWithoutId model
            , Cmd.none
            )

        AddMirrorButtonPressed ->
            ( model, Random.generate AddMirror (generateMirror model.room) )

        AddMirror mirrorWithoutId ->
            ( addMirror mirrorWithoutId model
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


addObject : (Id -> Object) -> Model -> Model
addObject objectWithoutId model =
    let
        id : Id
        id =
            model.nextId
    in
    { model
        | nextId = model.nextId + 1
        , objects = Dict.insert id (objectWithoutId id) model.objects
    }


addEye : (Id -> Eye) -> Model -> Model
addEye eyeWithoutId model =
    let
        id : Id
        id =
            model.nextId
    in
    { model
        | nextId = model.nextId + 1
        , eyes = Dict.insert id (eyeWithoutId id) model.eyes
    }


addMirror : (Id -> Mirror) -> Model -> Model
addMirror mirrorWithoutId model =
    let
        id : Id
        id =
            model.nextId
    in
    { model
        | nextId = model.nextId + 1
        , mirrors = Dict.insert id (mirrorWithoutId id) model.mirrors
    }


dragConfig : Draggable.Config SelectableComponentId Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy (\( dx, dy ) -> Vector2d.pixels dx dy |> OnDragBy)
        , Draggable.Events.onDragStart StartDragging
        , Draggable.Events.onDragEnd StopDragging
        ]


onDragBy : Model -> Vector2d Pixels Coordinates -> Model
onDragBy model delta =
    case model.currentlyDragging of
        Nothing ->
            -- Note: I think getting an onDragMsg if nothing is being dragged may represent a bug, and potentially something should be logged or this should be handled in some way
            model

        Just (ObjectSelected id) ->
            { model
                | objects =
                    Dict.update id
                        (Maybe.map (dragObject delta))
                        model.objects
            }

        Just (EyeSelected EyePosition id) ->
            -- not having an {eye/mirror} corresponding to a dragged id is very suprising. Currently that case is being silently ignored
            { model
                | eyes =
                    Dict.update id
                        (Maybe.map (dragEye model.lastMousePosition delta EyePosition))
                        model.eyes
            }

        Just (EyeSelected LightRay id) ->
            let
                lastMousePosition : Point2d Pixels Coordinates
                lastMousePosition =
                    model.lastMousePosition
                        |> Point2d.translateBy delta
            in
            { model
                | lastMousePosition = lastMousePosition
                , eyes =
                    Dict.update id
                        (Maybe.map (dragEye lastMousePosition delta LightRay))
                        model.eyes
            }

        Just (MirrorSelected component id) ->
            { model
                | mirrors =
                    Dict.update id
                        (Maybe.map (dragMirror delta component))
                        model.mirrors
            }


dragObject : Vector2d Pixels Coordinates -> Object -> Object
dragObject delta object =
    { object
        | position =
            object.position
                |> Point2d.translateBy delta
    }


dragEye : MousePosition -> Vector2d Pixels Coordinates -> EyeComponent -> Eye -> Eye
dragEye mousePosition delta component eye =
    case component of
        EyePosition ->
            { eye
                | position =
                    eye.position
                        |> Point2d.translateBy delta
            }

        LightRay ->
            case Direction2d.from eye.position mousePosition of
                Nothing ->
                    -- if the mouse position is exactly on the eye there is no direction. Current logic is to ignore this case an not update the light ray
                    eye

                Just directionToMouse ->
                    { eye | lightRay = directionToMouse }


dragMirror : Vector2d Pixels Coordinates -> MirrorComponent -> Mirror -> Mirror
dragMirror delta component mirror =
    let
        position : LineSegment2d Pixels Coordinates
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



-- SCENARIOS


type WhichScenario
    = Scenario1
    | Scenario2
    | Scenario3
    | Scenario4
    | Scenario5


scenario : WhichScenario -> Model
scenario whichScenario =
    case whichScenario of
        Scenario1 ->
            emptyModel
                |> addEye (Eye (Point2d.pixels 150 150) (Direction2d.degrees 45))
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 20 400)
                            (Point2d.pixels 800 400)
                        )
                    )

        Scenario2 ->
            emptyModel
                |> addEye (Eye (Point2d.pixels 400 400) (Direction2d.degrees 45))
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 100 650)
                            (Point2d.pixels 800 650)
                        )
                    )
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 450 150)
                            (Point2d.pixels 800 650)
                        )
                    )

        Scenario3 ->
            emptyModel
                |> addEye (Eye (Point2d.pixels 400 400) (Direction2d.degrees 45))
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 100 650)
                            (Point2d.pixels 800 650)
                        )
                    )
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 800 200)
                            (Point2d.pixels 800 650)
                        )
                    )

        Scenario4 ->
            emptyModel
                |> addEye (Eye (Point2d.pixels 400 400) (Direction2d.degrees 110))
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 100 650)
                            (Point2d.pixels 800 650)
                        )
                    )
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 800 200)
                            (Point2d.pixels 800 650)
                        )
                    )
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 100 200)
                            (Point2d.pixels 100 650)
                        )
                    )
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 800 200)
                            (Point2d.pixels 200 200)
                        )
                    )

        Scenario5 ->
            emptyModel
                |> addEye (Eye (Point2d.pixels 200 300) (Direction2d.degrees 70))
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 100 250)
                            (Point2d.pixels 700 250)
                        )
                    )
                |> addMirror
                    (Mirror
                        (LineSegment2d.from (Point2d.pixels 100 400)
                            (Point2d.pixels 700 400)
                        )
                    )
