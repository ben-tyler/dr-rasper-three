module Sprite exposing (..)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (style, src)
import Array exposing (Array)

type alias Model = 
    { sprites : Array String
    , currentSpriteIndex : Int
    , lastFrameTime : Float
    , frameDuration : Float
    }

type Animate = Static String | Animated  

type Msg = AnimateSprite Float

update : Msg -> Model -> Model
update msg model =
    case msg of
        AnimateSprite time ->
            let
                elapsed = time + model.lastFrameTime
                (newIndex, newtime) =
                    if elapsed >= model.frameDuration then
                        if (model.currentSpriteIndex + 1) == (Array.length model.sprites) then 
                            (0, time)
                        else 
                            (model.currentSpriteIndex + 1, time)
                    else
                        (model.currentSpriteIndex, elapsed)
            in
            { model
                | currentSpriteIndex = newIndex
                , lastFrameTime = newtime
            }

view {currentSpriteIndex, sprites} nodeDirection = 
    case  Array.get currentSpriteIndex sprites of 
        Just currentSpritePath -> 
            div 
                [style "position" "relative"]
                [ img
                    [ src currentSpritePath
                    , style "width" "100px"
                    , style "height" "100px" -- Adjust size as needed
                    , style "transform" <| "scaleX(" ++ nodeDirection ++ ")"
                    ]
                    []
                ]
        Nothing -> 
            div [] [text "No Sprite Loaded"]

playerIdle : Model 
playerIdle = 
    { sprites =
        Array.fromList
            [ "assets/BanditIdle/Idle_000.png"
            , "assets/BanditIdle/Idle_001.png"
            , "assets/BanditIdle/Idle_002.png"
            , "assets/BanditIdle/Idle_003.png"
            , "assets/BanditIdle/Idle_004.png"
            , "assets/BanditIdle/Idle_005.png"
            , "assets/BanditIdle/Idle_006.png"
            , "assets/BanditIdle/Idle_007.png"
            , "assets/BanditIdle/Idle_008.png"
            , "assets/BanditIdle/Idle_009.png"
            , "assets/BanditIdle/Idle_010.png"
            , "assets/BanditIdle/Idle_011.png"
            , "assets/BanditIdle/Idle_012.png"
            , "assets/BanditIdle/Idle_013.png"
            , "assets/BanditIdle/Idle_014.png"
            , "assets/BanditIdle/Idle_015.png"
            , "assets/BanditIdle/Idle_016.png"
            , "assets/BanditIdle/Idle_017.png"
            ]
    , currentSpriteIndex = 0
    , lastFrameTime = 0
    , frameDuration = 100
    }


playerWalking : Model
playerWalking =
    { sprites =
        Array.fromList
            [ "Walking/Walking_000.png" -- Replace these with the paths to your sprite images
            , "Walking/Walking_001.png"
            , "Walking/Walking_002.png"
            , "Walking/Walking_003.png" -- Replace these with the paths to your sprite images
            , "Walking/Walking_004.png"
            , "Walking/Walking_005.png"
            , "Walking/Walking_006.png" -- Replace these with the paths to your sprite images
            , "Walking/Walking_007.png"
            , "Walking/Walking_008.png"
            , "Walking/Walking_009.png" -- Replace these with the paths to your sprite images
            , "Walking/Walking_010.png"
            , "Walking/Walking_011.png"
            , "Walking/Walking_012.png"
            , "Walking/Walking_013.png"
            , "Walking/Walking_014.png"
            , "Walking/Walking_015.png"
            , "Walking/Walking_016.png"
            , "Walking/Walking_017.png"
            , "Walking/Walking_018.png"
            , "Walking/Walking_019.png"
            , "Walking/Walking_020.png"
            , "Walking/Walking_021.png"
            , "Walking/Walking_022.png"
            , "Walking/Walking_023.png"
            , "Walking/Walking_024.png"
            ]
    , currentSpriteIndex = 0
    , lastFrameTime = 0
    , frameDuration = 100 -- Milliseconds per frame; adjust as needed
    }


witchIdle : Model 
witchIdle = 
    { sprites =
        Array.fromList
            [ "assets/WitchIdle/Idle_000.png"
            , "assets/WitchIdle/Idle_001.png"
            , "assets/WitchIdle/Idle_002.png"
            , "assets/WitchIdle/Idle_003.png"
            , "assets/WitchIdle/Idle_004.png"
            , "assets/WitchIdle/Idle_005.png"
            , "assets/WitchIdle/Idle_006.png"
            , "assets/WitchIdle/Idle_007.png"
            , "assets/WitchIdle/Idle_008.png"
            , "assets/WitchIdle/Idle_009.png"
            , "assets/WitchIdle/Idle_010.png"
            , "assets/WitchIdle/Idle_011.png"
            , "assets/WitchIdle/Idle_012.png"
            , "assets/WitchIdle/Idle_013.png"
            , "assets/WitchIdle/Idle_014.png"
            , "assets/WitchIdle/Idle_015.png"
            , "assets/WitchIdle/Idle_016.png"
            , "assets/WitchIdle/Idle_017.png"
            ]
    , currentSpriteIndex = 0
    , lastFrameTime = 0
    , frameDuration = 100
    }

witchWalking : Model 
witchWalking = 
    { sprites =
        Array.fromList
            [ "assets/WitchWalking/Walking_000.png" -- Replace these with the paths to your sprite images
            , "assets/WitchWalking/Walking_001.png"
            , "assets/WitchWalking/Walking_002.png"
            , "assets/WitchWalking/Walking_003.png" -- Replace these with the paths to your sprite images
            , "assets/WitchWalking/Walking_004.png"
            , "assets/WitchWalking/Walking_005.png"
            , "assets/WitchWalking/Walking_006.png" -- Replace these with the paths to your sprite images
            , "assets/WitchWalking/Walking_007.png"
            , "assets/WitchWalking/Walking_008.png"
            , "assets/WitchWalking/Walking_009.png" -- Replace these with the paths to your sprite images
            , "assets/WitchWalking/Walking_010.png"
            , "assets/WitchWalking/Walking_011.png"
            , "assets/WitchWalking/Walking_012.png"
            , "assets/WitchWalking/Walking_013.png"
            , "assets/WitchWalking/Walking_014.png"
            , "assets/WitchWalking/Walking_015.png"
            , "assets/WitchWalking/Walking_016.png"
            , "assets/WitchWalking/Walking_017.png"
            , "assets/WitchWalking/Walking_018.png"
            , "assets/WitchWalking/Walking_019.png"
            , "assets/WitchWalking/Walking_020.png"
            , "assets/WitchWalking/Walking_021.png"
            , "assets/WitchWalking/Walking_022.png"
            , "assets/WitchWalking/Walking_023.png"
            , "assets/WitchWalking/Walking_024.png"
            ]
    , currentSpriteIndex = 0
    , lastFrameTime = 0
    , frameDuration = 100 -- Milliseconds per frame; adjust as needed
    }



kingIdle : Model 
kingIdle = 
    { sprites =
        Array.fromList
            [ "assets/KingIdle/Idle_000.png"
            , "assets/KingIdle/Idle_001.png"
            , "assets/KingIdle/Idle_002.png"
            , "assets/KingIdle/Idle_003.png"
            , "assets/KingIdle/Idle_004.png"
            , "assets/KingIdle/Idle_005.png"
            , "assets/KingIdle/Idle_006.png"
            , "assets/KingIdle/Idle_007.png"
            , "assets/KingIdle/Idle_008.png"
            , "assets/KingIdle/Idle_009.png"
            , "assets/KingIdle/Idle_010.png"
            , "assets/KingIdle/Idle_011.png"
            , "assets/KingIdle/Idle_012.png"
            , "assets/KingIdle/Idle_013.png"
            , "assets/KingIdle/Idle_014.png"
            , "assets/KingIdle/Idle_015.png"
            , "assets/KingIdle/Idle_016.png"
            , "assets/KingIdle/Idle_017.png"
            ]
    , currentSpriteIndex = 0
    , lastFrameTime = 0
    , frameDuration = 100
    }

kingWalking : Model 
kingWalking = 
    { sprites =
        Array.fromList
            [ "assets/kingWalking/Walking_000.png" -- Replace these with the paths to your sprite images
            , "assets/kingWalking/Walking_001.png"
            , "assets/kingWalking/Walking_002.png"
            , "assets/kingWalking/Walking_003.png" -- Replace these with the paths to your sprite images
            , "assets/kingWalking/Walking_004.png"
            , "assets/kingWalking/Walking_005.png"
            , "assets/kingWalking/Walking_006.png" -- Replace these with the paths to your sprite images
            , "assets/kingWalking/Walking_007.png"
            , "assets/kingWalking/Walking_008.png"
            , "assets/kingWalking/Walking_009.png" -- Replace these with the paths to your sprite images
            , "assets/kingWalking/Walking_010.png"
            , "assets/kingWalking/Walking_011.png"
            , "assets/kingWalking/Walking_012.png"
            , "assets/kingWalking/Walking_013.png"
            , "assets/kingWalking/Walking_014.png"
            , "assets/kingWalking/Walking_015.png"
            , "assets/kingWalking/Walking_016.png"
            , "assets/kingWalking/Walking_017.png"
            , "assets/kingWalking/Walking_018.png"
            , "assets/kingWalking/Walking_019.png"
            , "assets/kingWalking/Walking_020.png"
            , "assets/kingWalking/Walking_021.png"
            , "assets/kingWalking/Walking_022.png"
            , "assets/kingWalking/Walking_023.png"
            , "assets/kingWalking/Walking_024.png"
            ]
    , currentSpriteIndex = 0
    , lastFrameTime = 0
    , frameDuration = 100 -- Milliseconds per frame; adjust as needed
    }

rock1 : Model
rock1 =
    { sprites =
        Array.fromList
            [ "assets/textures/Rocks/downsized/Rocks_02_Fill.png"
            ]
    , currentSpriteIndex = 0
    , lastFrameTime = 0
    , frameDuration = 100
    }

rock2 : Model
rock2 =
    { sprites =
        Array.fromList
            [ "assets/textures/Rocks/downsized/Rocks_03_Fill.png"
            ]
    , currentSpriteIndex = 0
    , lastFrameTime = 0
    , frameDuration = 100
    }
