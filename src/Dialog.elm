module Dialog exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type Dialog
    = StartDialog
    | Greeting
    | Farewell
    | HappyResponse
    | SadResponse
    | EndDialog

type alias CharacterDialog =
    { initialDialog : Dialog
    , dialogText : Dialog -> String
    , responses : Dialog -> List Dialog
    , responseText : Dialog -> String
    , isEndDialog : Dialog -> Bool
    }

character1Dialog : CharacterDialog
character1Dialog =
    { initialDialog = StartDialog
    , dialogText =
        \dialog ->
            case dialog of
                StartDialog ->
                    "Welcome! Would you like to chat?"

                Greeting ->
                    "Hello! Are you feeling happy or sad today?"

                HappyResponse ->
                    "Great to hear! Have a nice day."

                SadResponse ->
                    "I hope your day gets better soon!"

                Farewell ->
                    "Goodbye! See you later."

                EndDialog ->
                    "End of conversation."
    , responses =
        \dialog ->
            case dialog of
                StartDialog ->
                    [ Greeting ]

                Greeting ->
                    [ HappyResponse, SadResponse ]

                HappyResponse ->
                    []

                SadResponse ->
                    []

                Farewell ->
                    []

                EndDialog ->
                    []
    , responseText =
        \dialog ->
            case dialog of
                HappyResponse ->
                    "Happy"

                SadResponse ->
                    "Sad"

                _ ->
                    "Next"
    , isEndDialog =
        \dialog ->
            dialog == EndDialog
    }

-- Model
type alias Model =
    { currentDialog : Dialog
    , isDialogActive : Bool
    }

initialModel : Model
initialModel =
    { currentDialog = character1Dialog.initialDialog
    , isDialogActive = False
    }

-- Msg
type Msg
    = SelectDialog Dialog
    | CloseDialog

-- Update
update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectDialog dialog ->
            { model | currentDialog = dialog, isDialogActive = True }

        CloseDialog ->
            { model | isDialogActive = False }

-- View
view : Model -> CharacterDialog -> Html Msg
view model characterDialog =
    let
        currentDialogText =
            characterDialog.dialogText model.currentDialog

        responseButtons =
            if model.isDialogActive then
                let
                    responses = characterDialog.responses model.currentDialog
                in
                List.map
                    (\response ->
                        button [ onClick (SelectDialog response) ] [ text (characterDialog.responseText response) ]
                    )
                    responses
            else
                [ button [ onClick (SelectDialog characterDialog.initialDialog) ] [ text "Start Dialog" ] ]
    in
    div []
        [ div [] [ text currentDialogText ]
        , div [] responseButtons
        ]
