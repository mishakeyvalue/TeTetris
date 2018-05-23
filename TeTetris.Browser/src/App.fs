module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

importAll "../sass/main.sass"

open Fable.Helpers.React
open Fable.Helpers.React.Props

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
            [ div
                [ ClassName "columns" ]
                [ div
                    [ ClassName "column is-3" ]
                    [ str "Hello, guys" ]
                ] ] ] ]
let init arg = (), []

let update _ b = b, []

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
