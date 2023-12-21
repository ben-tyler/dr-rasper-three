module Map exposing (..)
import Node
import Domain
import Sprite
import Html exposing (Html, div, img, text)
import Html exposing (col)
import Node exposing (Collision)

type alias Model = 
    { map : List Node.Model
    }

type Msg = Node Node.Msg

view : Model -> Domain.Camera -> Html Msg
view model camera = 
    List.map
        ( \i -> Node.view i camera |> Html.map Node) 
        model.map
        |> div []

parseMap : String -> List Node.Model
parseMap mapString =
    let
        rows =
            String.lines mapString

        addRow : Int -> String ->  List Node.Model
        addRow rowIdx row =
            let
                colChars =
                    String.toList row
            in
            List.indexedMap
                (\colIdx char ->
                    let
                        pos = Domain.Position (toFloat colIdx * 100) (toFloat rowIdx * 100)
                    in
                    case char of 
                        '1' ->
                            Node.nodeBuilder ("map" ++ String.fromInt colIdx) [("Idle", Sprite.rock1)] pos False 70

                        _ ->
                            Node.nodeBuilder ("map" ++ String.fromInt colIdx) [("Idle", Sprite.rock2)] pos True 70
                )
                colChars
    in
    List.indexedMap addRow rows |> List.concat


map2 = """
010
101
"""

map1 = 
 """
000000000000000000000000
000000000000000000000000
001101111111111111111100
001101111100111111111100
001111111100111111111100
000001111100111111111100
001111111100111111111100
000000000000000000000000
000000000000000000000000
"""
