module Keys exposing (..)
import Domain

noKeys : Domain.Keys
noKeys =
  Domain.Keys False False False False False

rightKeys : Domain.Keys
rightKeys =
  Domain.Keys False False False True False

downKeys : Domain.Keys
downKeys =
  Domain.Keys False False True False False

leftKeys : Domain.Keys
leftKeys =
  Domain.Keys False True False False False

moving : Domain.Keys -> Bool
moving keys =
    keys.down || keys.left || keys.right || keys.up


updateArrows : Bool -> String -> Domain.Keys -> Domain.Keys
updateArrows isDown key keys =
  case key of
    " "          -> { keys | space = isDown }
    "ArrowUp"    -> { keys | up    = isDown }
    "ArrowLeft"  -> { keys | left  = isDown }
    "ArrowDown"  -> { keys | down  = isDown }
    "ArrowRight" -> { keys | right = isDown }
    _            -> keys


updateWasd : Bool -> String -> Domain.Keys -> Domain.Keys
updateWasd isDown key keys =
  case key of
    " "     -> { keys | space = isDown }
    "W"     -> { keys | up    = isDown }
    "A"     -> { keys | left  = isDown }
    "S"     -> { keys | down  = isDown }
    "D"     -> { keys | right = isDown }
    "w"     -> { keys | up    = isDown }
    "a"     -> { keys | left  = isDown }
    "s"     -> { keys | down  = isDown }
    "d"     -> { keys | right = isDown }
    _       -> keys


moveByKey : Domain.Keys -> Float -> Domain.Position ->  Domain.Position
moveByKey keys speed pos = 
    pos
        |> (\ {x, y} -> 
                if keys.left then 
                    Domain.Position (x - speed) y
                else 
                    Domain.Position x y
            )
        |> (\ {x, y}  -> 
                if keys.right then 
                    Domain.Position (x + speed) y
                else 
                    Domain.Position x y
            )
        |> (\ {x, y}  -> 
                if keys.up then 
                    Domain.Position x (y - speed)
                else 
                    Domain.Position x y
            )
        |> (\ {x, y}   -> 
                if keys.down then 
                    Domain.Position x  (y + speed)
                else 
                    Domain.Position x y
            )
