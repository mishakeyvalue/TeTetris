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

open TeTetris.Core
open Fable.PowerPack.Keyboard

// MODEL

type Msg =
    | StartGame
    | Tick

// UPDATE
let init _ = NotStarted, []
let RefreshRate = 512 / 2

let mutable interval = 0.

let tick dispatch =
    interval <- window.setInterval((fun _ -> 
        window.console.log "Hello, motherfucker "
        dispatch Tick), RefreshRate)

let stop _ =
    window.clearInterval (interval)


let tetraminos = 
    [
        Palka
        Cube
        Palka
    ]

let initialState = emptyState tetraminos

let update msg model = 
    match msg with
        | StartGame -> InProgress initialState, Cmd.ofSub tick
        | Tick -> match model with
            | InProgress state -> InProgress (gameTick state), []
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
let point color p =
  printfn "%A" p
  rect
     [width Scale
      height Scale
      X ((p.x) * Scale)
      Y ((WorldHeight - p.y - 1) * Scale)
      fill color
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
            | InProgress s -> s.activeTetramino.coords |> List.map (point "green")
            | _            -> []
         )
          
          @
          (match model with
            | InProgress s -> s.blocks |> List.map (point "black")
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
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
