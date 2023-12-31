module Node exposing (..)
import Sprite
import Html exposing (Html, div, img, text, h3)
import Html.Attributes exposing (style, src)
import Domain
import Keys
import Process exposing (Id)
import Dict exposing (Dict)
import String exposing (right)

type Msg 
    = Animate Float
    | Move Domain.Keys
    | ChangeState Domain.Keys

type Direction = Left | Right

type Instruction = NoInstruction | MoveTo Domain.Position
type EffectTrigger = NoEffect | StartDialog String Dialog 

type alias Model = 
    { id : String
    , position : Domain.Position
    , isCollision : Bool
    , boxSize : Float
    , direction : Direction
    , animations : Dict String Sprite.Model
    , animation : String
    , currentDialog : Dialog
    , instruction : Instruction
    , timer : Int
    }


nodeBuilder : String -> List (String, Sprite.Model) -> Domain.Position -> Bool -> Float -> Model
nodeBuilder id animations position isCollision boxSize = 
    { id = id
    , position= position
    , isCollision = isCollision
    , boxSize = boxSize
    , direction = Right
    , animations = Dict.fromList animations
    , animation = 
        case animations of
            ((s, _)::_) -> s 
            _ -> ""
    , currentDialog = dialogSystem
    , instruction = NoInstruction
    , timer = 0
    }

type Collided = RightColide Model | LeftColide Model
type Collision = NoCollide | Collides (List Collided) | ACollide Collided

nodeDirection : Direction -> String
nodeDirection direction = 
    case direction of 
        Left -> "-1"
        Right -> "1"
            

view : Model -> Domain.Camera -> Html Msg
view model camera = 
    let
        {x, y} = { x = model.position.x - camera.x, y = model.position.y - camera.y }
        sprite = Dict.get model.animation model.animations

        dir = nodeDirection model.direction
    in
    div 
        [ style "position" "absolute"
        , style "top" <| String.fromFloat y ++ "px"
        , style "left" <| String.fromFloat x ++ "px"
        ] 
        [ case sprite of 
            Just s -> Sprite.view s dir
            Nothing -> text "Unable to load animation"
        ]

runInstruction : Model -> Model 
runInstruction model = 
    case model.instruction of 
        NoInstruction -> 
            model

        MoveTo p -> 
            let
                fakeKeys = 
                    if model.position.x == p.x then 
                        Keys.downKeys
                    else 
                        if model.position.x < p.x then Keys.rightKeys else Keys.leftKeys
         
            in
            if p.x == model.position.x && p.y == model.position.y then 
                { model | instruction = NoInstruction }
                |> ( \ i -> update (ChangeState fakeKeys) i)
            else 
                let
                    position = model.position

                in
                
                { model | 
                    position = 
                        { x= if position.x < p.x then position.x + 1 else position.x - 1
                        , y = if position.y < p.y then position.y + 1 else position.y - 1
                        }

                }
                |> ( \ i -> update (ChangeState fakeKeys) i)

viewDialog : Domain.Position ->  Model -> Domain.Camera -> Html Msg
viewDialog respPos model camera = 
    let
        {x, y} = { x = model.position.x - camera.x, y = model.position.y - camera.y }
        rx = respPos.x - camera.x
        ry = respPos.y - camera.y
        delay = model.timer < 250
    in
    div [] 
        [ div 
            [ style "position" "absolute"
            , style "top" <| String.fromFloat (y - 100) ++ "px"
            , style "left" <| String.fromFloat x ++ "px"
            , style "background-color" "white"
            , style "padding" "2px"
            , style "width" "200px"
            , style "font-size" "14px"
            ] 
            [h3 [] [text <| case model.currentDialog of Dialog d -> d.statement]]
            
        ,if delay then 
            div [] []
         else
            div 
                [ style "position" "absolute"
                , style "top" <| String.fromFloat (ry - 10) ++ "px"
                , style "left" <| String.fromFloat rx ++ "px"
                , style "background-color" "white"
                , style "padding" "2px"
                , style "width" "200px"
                , style "font-size" "12px"
                ] 
                ( List.indexedMap 
                    ( \ i (s, d) -> div [] [text <| (String.fromInt <| i + 1) ++ " >> " ++ s] ) 
                    (case model.currentDialog of Dialog fd -> fd.responses)
                )
        ]



dialogChoice : Int -> Model -> Dialog
dialogChoice index model =    
    let
        d = case model.currentDialog of Dialog p -> p
    in
    
    case 
        List.take index d.responses               -- [ 1, 2, 3 ]
            |> List.reverse                    -- [ 3, 2, 1 ]
            |> List.head 
    of
        Just (_, a) -> 
            a

        Nothing -> 
            model.currentDialog



update : Msg -> Model -> Model
update msg model =
    case msg of
        Animate time ->
            case Dict.get model.animation model.animations of 
                Just s -> 
                    { model 
                        | animations = Dict.insert model.animation (Sprite.update (Sprite.AnimateSprite time) s) model.animations 
                        , timer = model.timer + 1
                    }
                Nothing -> 
                    { model | timer = model.timer + 1 }  

        Move keys ->
            { model | position = Keys.moveByKey keys 1 model.position}

        ChangeState keys -> 
            let
                foo =
                    if Keys.moving keys && model.animation == "Idle" then 
                        "Walking"
                    else if not <| Keys.moving keys && model.animation == "Walking" then 
                        "Idle"
                    else
                        model.animation

                foo2 = 
                    if keys.right && model.direction == Left then 
                        Right
                    else if keys.left && model.direction == Right then 
                        Left
                    else 
                        model.direction
            in
            { model | animation = foo, direction = foo2 }
            

checkACollision : Model -> Model -> Collision
checkACollision model check = 
    let 
        (bx, by) = (check.position.x, check.position.y)
        x = model.position.x
        y = model.position.y
        collive =  x > bx - check.boxSize && x < bx + check.boxSize && y > by - check.boxSize && y < by + check.boxSize && check.isCollision
    in
    if collive then 
        let
            collisionX = model.position.x
            halfCollisionBoxSize = model.boxSize / 2.0
            collisionLeftSide = collisionX - halfCollisionBoxSize
            collisionRightSide = collisionX + halfCollisionBoxSize
        in
        if x < collisionLeftSide then
            ACollide <| RightColide model
        else
            ACollide <| LeftColide model
    else 
        NoCollide


checkCollisions : Model -> List Model -> Collision
checkCollisions model nodesToCheck = 
    let
        {x, y} = model.position
        collisions = 
            List.filter (\data -> 
                let (bx, by) = (data.position.x, data.position.y)
                in x > bx - data.boxSize && x < bx + data.boxSize && y > by - data.boxSize && y < by + data.boxSize && data.isCollision
            ) nodesToCheck
            |> List.map ( \ item -> 
                    let
                        collisionX = item.position.x
                        halfCollisionBoxSize = item.boxSize / 2.0
                        collisionLeftSide = collisionX - halfCollisionBoxSize
                        collisionRightSide = collisionX + halfCollisionBoxSize
                    in
                    if x < collisionLeftSide then
                        RightColide item
                    else
                        LeftColide item
                )
    in
    case collisions of 
        [] ->  NoCollide
        items -> Collides items

type Dialog = Dialog DialogPart

type alias DialogPart = 
    { statement : String
    , responses : List ( String, Dialog)
    , trigger : Instruction
    , effectTrigger : EffectTrigger
    }


offToWork = 
    buildWE
        "well, "
        []
        (StartDialog 
            "king" 
            (buildWT
                "I have had trouble inserting this eye through my eyes holes"
                [ ("my god what is wrong with you", buildWT "what nothing just my bloody eye hole" [] (MoveTo {x = 620, y = 200} ) ) 
                ]
                (MoveTo {x = 620, y = 200} )
            )
        )

foobar = 
    buildWT
        "Yes yes, come this way. I think you will be happy with how we are progressing with the preperations"
        [   ("Thank you janice, you always meant a lot to me"
            , buildBD 
                "not now Dr Rasper, you have work to do"
                [   ("yes... of course "
                    , buildWT
                        "well then get to it"
                        [ ( "yes.. yes ok janice", offToWork)

                        ]
                        (MoveTo {x = 620, y = 500} )
                    )

                ] 

            )
        ,   ("Oh janice, you know I never thought much of you"
            , buildWT
                "hmmmm"
                [ ("well, i shoulf get to it", offToWork)
                ]
                (MoveTo {x = 620, y = 500} )
                
            )
        ]
        (MoveTo {x = 300, y = 500})

buildBD : String -> List (String, Dialog) -> Dialog
buildBD statement responses = 
    Dialog
        { statement = statement
        , responses = responses
        , trigger = NoInstruction
        , effectTrigger = NoEffect
        }

buildWT : String -> List (String, Dialog) -> Instruction -> Dialog
buildWT statement responses trigger = 
    Dialog
        { statement = statement
        , responses = responses
        , trigger = trigger
        , effectTrigger = NoEffect
        }

buildWE : String -> List (String, Dialog) -> EffectTrigger -> Dialog
buildWE statement responses trigger = 
    Dialog
        { statement = statement
        , responses = responses
        , trigger = NoInstruction
        , effectTrigger = trigger
        }


dialogSystem : Dialog
dialogSystem = 
    buildBD
        "Dr Rasper, nice to see you. I hope you are ready for the surgury"
        [ ("I have been studying..."
          , foobar
          )
        , ("As ready as a heart attack"
          , foobar
          )
        ]
