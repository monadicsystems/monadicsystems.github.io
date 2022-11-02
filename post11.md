---
title: Full-Stack Elm Architecture
image: /logo-bg.png
summary: Recreating the Elm Architecture in Pure Haskell
---

# Full-Stack Elm Architecture

```elm
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = Int

init : Model
init = 0

-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1
    Decrement ->
      model - 1

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
```

```haskell
pattern Increment :: (Method, Path)
pattern Increment = (GET, ["increment"])

pattern Decrement :: (Method, Path)
pattern Decrement = (GET, ["decrement"])

route :: MonadServer m => ((Method, Path) -> m ()) -> m ()
route matcher = do
  requestMethod <- method
  requestPath   <- path
  matcher (requestMethod, requestPath)
  
data Msg = Msg (Method, Path)

data Trigger =
  Click
  | Key Char
  | ...
  
data Target =
  Id Text
  | Class Text
  | Element Text
  | ...

data Swap =
  Inner
  | Outer
  | ...

on :: Trigger -> Msg -> Target -> Swap -> [Attribute]
on = undefined

main :: IO ()
main = run id $ route \case
  Increment -> do
  Decrement -> do
  _ -> next
```

