module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, input, text,span, label)
import Html.Events exposing (onClick, onInput,onCheck)
import Html.Attributes exposing (placeholder, value, type_,checked,name, id, class)
import List exposing (map)

-- Main Program
main =
    Browser.sandbox { init = init, update = update, view = view }


------------------------------------------------------------------------------------
-- Model ---------------------------------------------------------------------------
------------------------------------------------------------------------------------

type alias Model =
    { num1 : Int
    , num2 : Int
    , result : Int
    , selectedNumber: SelectedNumber
    , error: Maybe String
    }
type SelectedNumber
    = Num1  
    | Num2
init : Model
init =
    { num1 = 0
    , num2 = 0
    , result = 0
    , selectedNumber = Num1
    , error = Nothing
    }


------------------------------------------------------------------------------------
-- Messages ------------------------------------------------------------------------
------------------------------------------------------------------------------------

type Msg
    = IncrementNum1
    | DecrementNum1
    | IncrementNum2
    | DecrementNum2
    | Clear
    | ChangeNum1 String
    | ChangeNum2 String
    | SwapNumbers
    | SelectNumber SelectedNumber
    | PerformOperation Operation
    | AppendNumber Int
    | AssignResultToNum1
    | AssignResultToNum2

type Operation
    = Add
    | Subtract
    | Multiply
    | Divide
    | Modulo

------------------------------------------------------------------------------------
-- Update --------------------------------------------------------------------------
------------------------------------------------------------------------------------

update : Msg -> Model -> Model
update msg model =
    case msg of
        IncrementNum1 ->
            { model | num1 = model.num1 + 1 }

        DecrementNum1 ->
            { model | num1 = model.num1 - 1 }

        IncrementNum2 ->
            { model | num2 = model.num2 + 1 }

        DecrementNum2 ->
            { model | num2 = model.num2 - 1 }
        Clear -> 
            { model | 
                num2 = 0
                ,num1 = 0
                ,result = 0
                ,error = Nothing
             }
        ChangeNum1 newNum1 ->
            let
                maybeNum = String.toInt newNum1
            in
            case maybeNum of
                Just num ->
                    if num > 100 then
                        { model | error = Just "Num1 not allowed higher than 100" }
                    else
                        { model | num1 = num, error = Nothing }

                Nothing ->
                    { model | error = Just "Invalid number input" }

        ChangeNum2 newNum2 ->
            let
                maybeNum = String.toInt newNum2
            in
            case maybeNum of
                Just num ->
                    if num > 100 then
                        { model | error = Just "Num2 not allowed higher than 100" }
                    else
                        { model | num2 = num, error = Nothing }

                Nothing ->
                    { model | error = Just "Invalid number input" }

        SelectNumber selectedNumber ->
            { model | selectedNumber = selectedNumber }
        SwapNumbers ->
            let 
                temp1 = model.num1
                temp2 = model.num2
            in
            { model | num1 = temp2, num2 = temp1 }
        AppendNumber number ->
            case model.selectedNumber of
                Num1 ->
                    let
                        newNum1 = String.fromInt model.num1 ++ String.fromInt number
                    in
                    case String.toInt newNum1 of
                        Just n ->
                            { model | num1 = n, error = Nothing }
                        Nothing ->
                            { model | error = Just "Invalid number concatenation" }

                Num2 ->
                    let
                        newNum2 = String.fromInt model.num2 ++ String.fromInt number
                    in
                    case String.toInt newNum2 of
                        Just n ->
                            { model | num2 = n, error = Nothing }
                        Nothing ->
                            { model | error = Just "Invalid number concatenation" }
        AssignResultToNum1 ->
            { model | num1 = model.result }
        AssignResultToNum2 ->
            { model | num2 = model.result }
        PerformOperation operation ->
           case operation of
                Add ->
                    { model | result = model.num1 + model.num2, error = Nothing }

                Subtract ->
                    { model | result = model.num1 - model.num2, error = Nothing }

                Multiply ->
                    { model | result = model.num1 * model.num2, error = Nothing }

                Divide ->
                    if model.num2 == 0 then
                        { model | result = 0, error = Just "Cannot divide by zero" }
                    else
                        { model | result = model.num1 // model.num2, error = Nothing }

                Modulo ->
                    if model.num2 == 0 then
                        { model | result = 0, error = Just "Cannot perform modulo operation with 0" }
                    else
                        { model | result = modBy model.num2 model.num1, error = Nothing }



------------------------------------------------------------------------------------
-- View ----------------------------------------------------------------------------
------------------------------------------------------------------------------------

view : Model -> Html Msg
view model =
    div [ id "elm-calculator"]
        [ 
            div [ id "calculator-figures"] [
                div [ id "result"] [ 
                    case model.error of
                        Just err -> div [ class "error-msg"] [ text err ]
                        Nothing -> div [ ] [
                            span [] [text "Result:"  ]
                            ,span [] [text (String.fromInt model.result)]]
                    ]
                , renderNumberControl "1" model.num1 ChangeNum1 IncrementNum1 DecrementNum1 (SelectNumber Num1)
                , renderNumberControl "2" model.num2 ChangeNum2 IncrementNum2 DecrementNum2 (SelectNumber Num2)
                
            ]
            , div [ class "calculator-body"] [
                div [ class "left-elements"] [
                    div [ class "operations-container "] [ 
                            button [ onClick (SwapNumbers) ] [ text "swap" ]
                            ,button [ onClick (Clear) ] [ text "clear" ]
                        ]
                    , renderSlider model.selectedNumber
                    , renderNumberButtons
                    ]
                , div [ class "right-elements"] [
                    renderOperationButtons
                ]
            ]
            , div [ class "calculator-bottom" ] [
                renderAssignResultButton "Assign result to number 1" AssignResultToNum1
                ,renderAssignResultButton "Assign result to number 2" AssignResultToNum2
            ]
        ]


------------------------------------------------------------------------------------
-- Helper Function -----------------------------------------------------------------
------------------------------------------------------------------------------------

renderNumberButtons : Html Msg
renderNumberButtons =
    div [id "number-btn-container"]
        (List.map
                    (\n -> button [ class "fixed-btn", onClick (AppendNumber n) ] [ text (String.fromInt n) ])
                    (List.range 1 9 ++ [0])
                )
renderSlider : SelectedNumber -> Html Msg
renderSlider selectedNumber =
    div [ class "slider-container" ]
        [ button
            [ class "slider-toggle"
            , onClick (SelectNumber (if selectedNumber == Num1 then Num2 else Num1))
            ]
            [ text (if selectedNumber == Num1 then "Number 1" else "Number 2") ]
        ]


renderAssignResultButton: String -> Msg -> Html Msg
renderAssignResultButton label onClickAction =
     button [ onClick onClickAction] [ text label]

renderOperationButtons : Html Msg
renderOperationButtons =
    div [ ]
        [ button [ class "fixed-btn", onClick (PerformOperation Add) ] [ text "+" ]
        , button [ class "fixed-btn", onClick (PerformOperation Subtract) ] [ text "-" ]
        , button [ class "fixed-btn", onClick (PerformOperation Multiply) ] [ text "*" ]
        , button [ class "fixed-btn", onClick (PerformOperation Divide) ] [ text "/" ]
        , button [ class "fixed-btn", onClick (PerformOperation Modulo) ] [ text "%" ]
        ]

renderNumberControl : String -> Int -> (String -> Msg) -> Msg -> Msg -> Msg -> Html Msg
renderNumberControl label numValue onChange onIncrement onDecrement onClickEvent =
    div [ class "number-row"]
        [ span [] [ text (label ++ ": ") ]
        , input
            [ placeholder "Enter a number"
            , value (String.fromInt numValue) -- Ensure value is converted to String
            , onInput onChange
            , onClick onClickEvent
            ]
            []
        , button [ onClick onIncrement ] [ text "+" ]
        , button [ onClick onDecrement ] [ text "-" ]
        ]