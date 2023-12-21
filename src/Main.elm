module Main exposing (..)

import Browser
import Html exposing (Html, div, img, text)
import Browser.Events as E
import Html.Attributes exposing (style, src)
import Array exposing (Array)
import Sprite
import Json.Decode as D
import Keys
import Node
import Domain
import Map
import Combat
-- Model

type GameState = Exploring | InCombat Combat.Model| InConversation String

type alias Model =
    { player : Node.Model
    , npcs : List Node.Model
    , arrows : Domain.Keys
    , wasd : Domain.Keys
    , fps : Float
    , camera : Domain.Camera
    , map : Map.Model
    , gameState : GameState
    }

-- Update

type Msg
    = AnimationFrame Float
    | KeyChanged Bool String
    | Node Node.Msg
    | Map Map.Msg
    | Combat Combat.Msg
    | KeyPressed String


handleWitch : Model -> Model 
handleWitch model = 
    model



gameLoop : Float -> Model -> Model 
gameLoop time model = 
    let
        player = Node.update (Node.ChangeState model.arrows) model.player
        nextPossiblePlayerPosition = Node.update (Node.Move model.arrows) player
        newPosition = 
            nextPossiblePlayerPosition
            |>  Node.update (Node.Animate time)
            |>  ( \ i -> 
                    case Node.checkCollisions i ( model.npcs ++ model.map.map) of
                        Node.Collides _ ->  
                            player
                        _ -> 
                            i
                )

        (npcs, stateChages) = 
            List.map ( \ i -> 
                case model.gameState of 
                    InConversation id -> 
                        if i.id == id then 
                            (i, Nothing)
                        else 
                            (i, Nothing)

                    Exploring -> 
                        case Node.checkACollision nextPossiblePlayerPosition i of 
                            Node.ACollide _ ->
                                if i.id == "witch" then 
                                    (i, Just <| InConversation i.id)
                                else
                                    (i, Nothing)
                            _ ->
                                (i, Nothing)

                    _ -> 
                        (i, Nothing)
            
            ) model.npcs
            |> List.unzip

        gameState = 
            case List.filter (\ i -> i /= Nothing) stateChages of
                (Just x::_) -> x
                _ -> model.gameState
    in
    { model
        | player = newPosition
        , npcs = npcs
            |> List.map (Node.update (Node.Animate time)) 
            |> List.map (\ o -> Node.runInstruction o)
        , camera = 
            Keys.moveByKey model.wasd 1 model.camera
        , gameState = gameState
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        Combat m -> 
            case model.gameState of 
                InCombat rasp -> 
                    ({model | gameState = InCombat (Combat.update m rasp )}, Cmd.none)

                _ ->
                    (model, Cmd.none)

        Map _ -> 
            (model, Cmd.none)

        Node _ -> 
            (model, Cmd.none)

        AnimationFrame time ->
            let
                nextModel = model
                    |> ( \ t i -> { i | fps = 1000 / t }) time
                    |> gameLoop time

            in
            ( nextModel
            , Cmd.none
            )

        KeyChanged b s -> 
            ({ model 
                | arrows = Keys.updateArrows b s model.arrows 
                , wasd = Keys.updateWasd b s model.wasd
             }
            , Cmd.none
            )

        KeyPressed s -> 
            case model.gameState of 
                InConversation c -> 
                    ( case String.toInt s of 
                        Just i -> 
                            let
                                (npcs, effects) = 
                                    model.npcs
                                    |> List.map ( \ j -> 
                                            if j.id == c then 
                                                let next = Node.dialogChoice i j in
                                                ({ j 
                                                    | currentDialog = next
                                                    , instruction = case next of Node.Dialog n -> n.trigger
                                                    , timer = 0
                                                }, 
                                                (case next of Node.Dialog n -> n.effectTrigger)
                                                )
                                            else
                                                (j, Node.NoEffect)
                                        ) 
                                    |> List.unzip
                                runEffect = 
                                    case List.filter (\ igf -> igf /= Node.NoEffect) effects of
                                        (x::_) -> 
                                            case x of 
                                                Node.NoEffect -> 
                                                    { model | npcs = npcs }

                                                Node.StartDialog idstring d -> 
                                                    { model 
                                                        | gameState = InConversation idstring
                                                        , npcs = npcs
                                                            |> List.map ( \ gfds -> 
                                                                if gfds.id == idstring then 
                                                                    { gfds | currentDialog = d }
                                                                else 
                                                                    gfds
                                                            
                                                                )
                                                    }

                                        _ -> { model | npcs = npcs }

                                

                            in
                            runEffect

                        _ -> model

                    , Cmd.none
                    )
                
                _ -> 
                    (model, Cmd.none)


-- View
view : Model -> Html Msg
view model =
    let
        toRender = 
            model.player :: model.npcs
            |> List.sortBy ( \ i -> i.position.y)
    in
    div [
            style "padding" "5%"
        ]
        [ div [] [text <| "FPS : " ++ String.fromFloat model.fps ]
        , div 
            [ style "position" "relative"
            , style "overflow" "hidden"
            , style "height" "400px"
            , style "width" "600px"
            ]
            ( (Map.view model.map model.camera |> Html.map Map)
            :: (List.map ( \ i -> Node.view i model.camera |> Html.map Node) toRender)
            ++ [case model.gameState of 
                    Exploring -> 
                        div [] []
                    InCombat c -> 
                        div [style "position" "absolute"
                            , style "top" <| String.fromFloat 100 ++ "px"
                            , style "left" <| String.fromFloat 200 ++ "px"

                            ] 
                            [ Combat.view c |> Html.map Combat

                            ]

                    InConversation c -> 
                        div [] (
                            List.map ( \ i -> 
                                if i.id == c then 
                                    Node.viewDialog model.player.position i model.camera |> Html.map Node
                                else 
                                    div [] []
                            ) model.npcs
                        )
               ]
            )
            --, Dialog.view model.dialog Dialog.character1Dialog |> Html.map Dialog
       -- , div [] [text <| Debug.toString model.player]
        ]

-- Subscription

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ E.onAnimationFrameDelta AnimationFrame
        , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        , E.onKeyPress (D.map KeyPressed (D.field "key" D.string))
        ]

-- Main
init : () -> ( Model, Cmd Msg )
init _ =
    ( { player = Node.nodeBuilder 
            "player"
            [ ("Idle", Sprite.playerIdle )
            , ("Walking", Sprite.playerWalking)
            ]
            {x = 200, y = 300} 
            True 
            20
      , npcs = 
            [ Node.nodeBuilder 
                "witch"
                [ ("Idle", Sprite.witchIdle  )
                , ("Walking", Sprite.witchWalking)
                ]
                {x = 300, y = 320} 
                True 
                20
            , Node.nodeBuilder
                "king"
                [ ("Idle", Sprite.kingIdle  )
                , ("Walking", Sprite.kingWalking)
                ]
                {x = 600, y = 600} 
                True 
                20
            ]
      , arrows = Keys.noKeys
      , fps = 0
      , camera = {x = 50, y = 150}
      , wasd = Keys.noKeys
      , map = Map.Model <| Map.parseMap Map.map1
      , gameState = Exploring
      }
    , Cmd.none
    )

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }
