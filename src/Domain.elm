module Domain exposing (..)

type alias Camera = 
    { x : Float
    , y : Float
    }

type alias Position = 
    { x : Float
    , y : Float
    }

type alias Keys =
  { up : Bool
  , left : Bool
  , down : Bool
  , right : Bool
  , space : Bool
  }
