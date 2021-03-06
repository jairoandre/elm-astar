module Main exposing (..)

import Html exposing (Html)
import Graphics.Render exposing (..)
import Color exposing (rgb)
import AnimationFrame
import Time exposing (Time)
import Dict exposing (Dict)
import Array exposing (Array)
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { spots : Array Spot
    , walls : Dict Int Bool
    , openSet : List Int
    , closedSet : List Int
    , time : Time
    , draw : Bool
    , keepLooking : Bool
    }


type alias Spot =
    { f : Int
    , g : Int
    , h : Int
    }


type Msg
    = Tick Time
    | SetWall Int Float


cols : Int
cols =
    5


rows : Int
rows =
    5


sQtd : Int
sQtd =
    cols * rows


lastIdx : Int
lastIdx =
    sQtd - 1


indexRange : List Int
indexRange =
    List.range 0 lastIdx


gSize : Float
gSize =
    500


w : Float
w =
    gSize / (toFloat cols)


h : Float
h =
    gSize / (toFloat rows)


idxToCartesian : Int -> ( Int, Int )
idxToCartesian idx =
    ( idx % cols, idx // cols )


cartesianToIdx : ( Int, Int ) -> Int
cartesianToIdx coord =
    let
        ( x, y ) =
            coord
    in
        (y * cols) + x


checkEdges : ( Int, Int ) -> Bool
checkEdges coord =
    let
        ( x, y ) =
            coord
    in
        x > 0 && x < cols && y > 0 && y < rows


neighbors : Int -> List Int
neighbors idx =
    let
        ( x, y ) =
            idxToCartesian idx
    in
        List.map cartesianToIdx <| List.filter checkEdges [ ( x + 1, y ), ( x, y + 1 ), ( x - 1, y ), ( x, y - 1 ) ]


initModel : Model
initModel =
    Model
        (Array.repeat (cols * rows) (Spot 0 0 0))
        Dict.empty
        [ 0 ]
        []
        0
        False
        True


setWall : Int -> Cmd Msg
setWall idx =
    Random.generate (SetWall idx) (Random.float 0 1)


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch <| List.map setWall indexRange )


lowestF : Model -> Int -> Maybe Int -> Maybe Int
lowestF model idx mIdx =
    case mIdx of
        Nothing ->
            Just idx

        Just cIdx ->
            case (Array.get cIdx model.spots) of
                Nothing ->
                    Just idx

                Just cSpot ->
                    case (Array.get idx model.spots) of
                        Nothing ->
                            Just cIdx

                        Just iSpot ->
                            if iSpot.f < cSpot.f then
                                Just idx
                            else
                                Just cIdx


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SetWall idx rndVal ->
            let
                newWalls =
                    Dict.insert idx (rndVal < 0.0) model.walls

                draw =
                    sQtd == (Dict.size newWalls)
            in
                ( { model | walls = newWalls, draw = draw }, Cmd.none )

        Tick newTime ->
            if (List.member lastIdx model.closedSet) || (List.isEmpty model.openSet) then
                ( { model | keepLooking = False }, Cmd.none )
            else
                let
                    current =
                        List.foldl (lowestF model) Nothing model.openSet

                    ( newOpenSet, newClosedSet ) =
                        case current of
                            Nothing ->
                                ( model.openSet, model.closedSet )

                            Just c ->
                                ( List.filter (\s -> s /= c) model.openSet
                                , c :: model.closedSet
                                )
                in
                    ( { model
                        | time = newTime
                        , openSet = newOpenSet
                        , closedSet = newClosedSet
                      }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.draw && model.keepLooking then
        AnimationFrame.times Tick
    else
        Sub.none



--Sub.none


drawSpot : Model -> Int -> Spot -> Form Msg
drawSpot model idx spot =
    let
        ( x, y ) =
            ( idx % cols, idx // cols )

        sColor =
            case Dict.get idx model.walls of
                Nothing ->
                    rgb 255 0 0

                Just v ->
                    if v then
                        rgb 0 0 0
                    else if (List.member idx model.openSet) then
                        rgb 0 255 0
                    else if (List.member idx model.closedSet) then
                        rgb 0 0 255
                    else
                        rgb 255 255 255
    in
        rectangle w h
            |> filledAndBordered
                (solid <| sColor)
                1
                (solid <| rgb 0 0 0)
            |> position ( (toFloat x) * w + (w / 2), (toFloat y) * h + (h / 2) )


drawSpots : Model -> Form Msg
drawSpots model =
    List.indexedMap (drawSpot model) (Array.toList model.spots) |> group


view : Model -> Html Msg
view model =
    if model.draw then
        drawSpots model
            |> svg 0 0 gSize gSize
    else
        Html.text "Carregando..."
