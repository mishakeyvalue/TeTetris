module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Helpers.React
open Fable.Helpers.React.Props

open TeTetris.Game.Core
open TeTetris.Game.Types
open Fable.PowerPack.Keyboard

// MODEL

type Msg =
    | StartGame
    | Tick
    | LeftKeyPressed
    | RightKeyPressed
    | DownKeyPressed

// *****************************************************
let RefreshRate = 512 / 2

let mutable interval = 0.

let tick dispatch =
    interval <- window.setInterval((fun _ -> 
        dispatch Tick), RefreshRate)

let stop _ =
    window.clearInterval (interval)


///// key bindings

let [<Literal>] LEFT_KEY = 37.
let [<Literal>] UP_KEY = 38.
let [<Literal>] RIGHT_KEY = 39.
let [<Literal>] DOWN_KEY = 40.


let onGameKeyDown _ = 
    let sub dispatch =
        (fun (e : Fable.Import.Browser.KeyboardEvent) ->
            match e with
                | ev when ev.keyCode = LEFT_KEY -> dispatch (LeftKeyPressed)
                | ev when ev.keyCode = RIGHT_KEY -> dispatch (RightKeyPressed)
                | ev when ev.keyCode = DOWN_KEY -> dispatch (DownKeyPressed)
                | _ -> ()
            :> obj)
        |> window.addEventListener_keydown
    Cmd.ofSub sub
// *****************************************************



// UPDATE
let init _ = NotStarted, []
let tetraminos = 
    [
        Palka
        R
        S
        Z
        J
        L
        Cube
        L
        Palka
    ]

let initialState = emptyState tetraminos

let update msg model = 
    match msg with
        | StartGame -> InProgress initialState, Cmd.ofSub tick
        | Tick -> match model with
            | InProgress state -> InProgress (commandHandler GameCommand.Tick state), []
            | _  -> model, []

        | LeftKeyPressed -> match model with
            | InProgress state -> InProgress (commandHandler GameCommand.MoveLeft state), []
            | _  -> model, []

         | RightKeyPressed -> match model with
            | InProgress state -> InProgress (commandHandler GameCommand.MoveRight state), []
            | _  -> model, []
         | DownKeyPressed -> match model with
            | InProgress state -> InProgress (commandHandler GameCommand.ShiftDown state), []
            | _  -> model, []

        | _ -> model, []


// VIEW
let Scale = 30
let private attr name value = unbox (name, value)
let width n = attr "width" n
let height n = attr "height" n
let fill c = attr "fill" c
let stroke c = attr "stroke" c
let strokeWidth n = attr "strokeWidth" n
let point (p,bl) =
  rect
     [width Scale
      height Scale
      X ((p.x) * Scale)
      Y ((WorldHeight - p.y - 1) * Scale)
      fill bl.color
      stroke "white"
     ]
     []

importAll "../sass/main.sass"

open Fable.Helpers.React
open Fable.Helpers.React.Props
open System.Drawing

let root model dispatch =
  let navbar =
    nav
        [ ClassName "nav" ]
        [ div
            [ ClassName "nav-left" ]
            [ h1
                [ ClassName "nav-item is-brand title is-4" ]
                [ str "TeTetris" ] ]
        ]
  
  let widthSize = WorldWidth * Scale
  let heightSize = WorldHeight * Scale

  let toList t = [t.a; t.b; t.c; t.d;]

  let game = 
        [svg 
        [width widthSize
         height heightSize
        ] 
        ([rect
            [ width widthSize
              height heightSize
              fill "white"
              stroke "black"
            ] 
            [

            ]      

          ]
          @
         (match model with
            | InProgress s -> s.activeTetramino.coords |> toList |> List.map (fun x -> x, {color="green"}) |> List.map (point)
            | _            -> []
         )
          
          @
          (match model with
            | InProgress s ->
                s.blocks
                |> Map.toList
                |> List.collect (fun (k,v) -> v |> Map.toList |> List.map (fun (k',v') -> {x=k; y=k'}, v'))
                |> List.choose (function | (_, None) -> None | (v, Some x) -> Some (v, x))
                |> List.map point // TODO: move convertion to VM to utils
            | _            -> []
          )
        );
            

       div [] 
        (match model with
            | NotStarted -> [button [OnClick (fun _ -> dispatch StartGame)] [str "Start the Game!"]]
            | _ -> []
        )
        
      

    ]
  
  div
    []
    [ div
        [ ClassName "navbar-bg" ]
        [ div
            [ ClassName "container" ]
            [ navbar ] ]
      div
        [ ClassName "section" ]
        [ div
            [ ClassName "container" ]
            game ] ]







open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.withSubscription onGameKeyDown
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
