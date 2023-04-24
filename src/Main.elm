module Main exposing (..)

import Axis2d exposing (Axis2d)
import Browser
import Dict exposing ( Dict )
import Browser.Events
import Circle2d
import Direction2d exposing (Direction2d)
import Draggable
import Draggable.Events exposing (onDragBy, onDragStart)
import Element exposing (Element, alignLeft, centerX, column, el, padding, row, text)
import Element.Input as Input
import Element.Region as Region
import Geometry.Svg as Svg
import Html exposing (Html)
import Json.Decode as D
import LineSegment2d exposing (LineSegment2d, endPoint, startPoint)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)
import Svg.Events as Events exposing (..)
import Vector2d exposing (Vector2d)


-- CONSTANTS


imageSize : String
imageSize =
    "1500"


roomSize : Float
roomSize =
    1000


lightLength : Float
lightLength =
    roomSize



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

type LabeledId =
     ObjectId Id
     | MirrorId Id


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


type alias MousePosition =
    Point2d Pixels TopLeftCoordinates



-- CURSOR MODE: what does pressing the cursor do


type CursorMode
    = AddObject
    | ChooseLightRay MousePosition


-- INVENTORY : things that can be added to scene
type Inventory =
      UnplacedObject
     

-- MODEL


type alias Model =
    { room : Room
    , objects : Dict Id Object
    , mirrors : Dict Id Mirror
    , nextId : Int
    , cursorMode : Maybe CursorMode
    , inventory : List Inventory 
    , drag : Draggable.State LabeledId
    , currentlyDragging : Maybe LabeledId
    }




-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { room = Rectangle2d.from (Point2d.pixels 0 0) (Point2d.pixels roomSize roomSize)
      , objects =
            Dict.fromList [ ( 1, { id = 1
              , position = Point2d.pixels 50 70
              , lightRay = Direction2d.degrees 50
              }
 )            ]
      , mirrors =
           Dict.fromList [(2,  { id = 2, position = LineSegment2d.from (Point2d.pixels 200 0) (Point2d.pixels 200 500) } )
           ,(3,
            { id = 3, position = LineSegment2d.from (Point2d.pixels 0 400) (Point2d.pixels 400 400) }
           )
           ]
      , nextId = 4
      , cursorMode = Nothing
      ,inventory = [ UnplacedObject ]
              , drag = Draggable.init
              , currentlyDragging = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    column []
        [ el [ Region.heading 1, centerX ] (text "Reflection Exploration")
        , row [ alignLeft, padding 5 ]
            [ Element.html (viewScene model)
            , Element.row []
                [ Input.button []
                    { onPress = Just AddObjectButtonPressed
                    , label = text "Add Object"
                    }
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
        [Svg.node "g" []
        (viewRoom model.room
            :: List.concatMap (viewObject model) (Dict.values model.objects )
            ++ List.map
                viewMirror
                (Dict.values model.mirrors)
        )]


viewRoom : Room -> Svg msg
viewRoom room =
    Svg.rectangle2d [ Attributes.stroke "black", Attributes.fill "burlywood" ] room


viewMirror : Mirror -> Svg Msg
viewMirror mirror =
    Svg.lineSegment2d
        [ Attributes.stroke "grey"
        , Attributes.strokeWidth "5"
        , Draggable.mouseTrigger (MirrorId mirror.id) DragMsg
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
                    |> Vector2d.scaleTo (pixels lightLength)
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


viewObject : Model -> Object -> List (Svg Msg)
viewObject model object =
    let
        shape =
            Svg.circle2d
                [ Attributes.fill "blue",
                Draggable.mouseTrigger (ObjectId object.id ) DragMsg
                ]
                (Circle2d.withRadius (pixels 10)
                    object.position
                )
    in
    [ viewLightPath (Dict.values model.mirrors ) object, shape ]



-- UPDATE


type Msg
    = Noop
    | AddObjectButtonPressed
    | MouseClicked MousePosition
    | OnDragBy ( Vector2d Pixels TopLeftCoordinates )
    | DragMsg (Draggable.Msg LabeledId)
    | StartDragging LabeledId
    | StopDragging


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragMsg dragMsg ->
                 Draggable.update dragConfig dragMsg model
        StartDragging id ->
                      ({model | currentlyDragging = Just id}, Cmd.none)
        StopDragging ->
                     ({ model | currentlyDragging = Nothing }, Cmd.none)
        OnDragBy delta ->
                 (onDragBy model delta, Cmd.none)

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

dragConfig : Draggable.Config LabeledId Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy (\( dx, dy ) -> Vector2d.pixels dx dy |> OnDragBy)
        , Draggable.Events.onDragStart StartDragging
        ]

onDragBy : Model ->  Vector2d Pixels TopLeftCoordinates -> Model
onDragBy model delta =
         case model.currentlyDragging of
              Nothing ->
              -- Note: I think getting an onDragMsg if nothing is being dragged may represent a bug, and potentially something should be logged or this should be handled in some way
                      model
              Just (ObjectId id) ->
                          -- not having an object corresponding to a dragged id is very suprising. Currently that case is being silently ignored 
                   { model | objects = Dict.update id (Maybe.map (\object -> { object | position = Point2d.translateBy delta object.position }) ) model.objects}
                       
              Just (MirrorId id) ->
                          -- not having a mirror  corresponding to a dragged id is very suprising. Currently that case is being silently ignored 
                   { model | mirrors = Dict.update id (Maybe.map (\mirror-> { mirror | position =LineSegment2d.translateBy delta mirror.position }) ) model.mirrors}




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
                    let
                        newObject =
                            { id = model.nextId, position = lastPosition, lightRay = lightRay }
                    in
                    { model | nextId = model.nextId + 1, cursorMode = Nothing, objects = Dict.insert newObject.id newObject model.objects }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
              Draggable.subscriptions DragMsg model.drag
