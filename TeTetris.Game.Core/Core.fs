module TeTetris.Game.Core

open TeTetris.Utils
open TeTetris.Game.Types

// Predefined

let WorldWidth = 10
let WorldHeight = 22

let startPoint = { x=WorldWidth-5; y=WorldHeight }

let initTetramino = function
    | Cube ->
        { shape=Cube;
          block={color="green"}
          coords=
          {
            a=startPoint;
            b={ startPoint with x = startPoint.x+1 };
            c={ startPoint with y = startPoint.y+1 };
            d={ startPoint with y = startPoint.y+1; x = startPoint.x + 1 };
          }
        }
        
    | Palka ->
        { shape=Palka;
          block={color="green"};
          coords=
          {
            a=startPoint;
            b={ startPoint with y = startPoint.y+1 };
            c={ startPoint with y = startPoint.y+2 };
            d={ startPoint with y = startPoint.y+3 };               
          }       
        }

let emptyGrid = [for i in 0 .. WorldWidth do
                   yield  i, [for j in 0 .. WorldHeight + 4 do yield j, None] |> Map.ofList 
                ] |> Map.ofList

let emptyState (x::xs) = { tetraminoQueue= Seq.repeat xs; activeTetramino=initTetramino x; blocks=emptyGrid }

// Game logic

let moveTetramino (t: TetraminoCoords) = 
    let movePoint p = {p with y = p.y-1}
    {t with 
        a = movePoint t.a
        b = movePoint t.b
        c = movePoint t.c
        d = movePoint t.d
    }

let isLandConflict (t: TetraminoCoords) (landed: Map<int, Map<int, Block option>>) = 
    let isPointConflict p = p.y < 0 || landed.[p.x].[p.y] |> Option.isSome
    isPointConflict t.a || isPointConflict t.b || isPointConflict t.c || isPointConflict t.d


let landTetramino state = 
     let addPoint p (blocks: Map<int, Map<int, Block option>>) = blocks.Add(p.x, (blocks.[p.x].Add( p.y, { color="black" } |> Some)))     
     let t = state.activeTetramino.coords
     let landedBlocks = state.blocks |> addPoint t.a |> addPoint t.b |> addPoint t.c |> addPoint t.d

     { state with 
         activeTetramino = state.tetraminoQueue |> Seq.head |> initTetramino
         blocks = landedBlocks
     }

let gameTick (state: State)=
    let potentialTetraminoPos = moveTetramino state.activeTetramino.coords
    if isLandConflict potentialTetraminoPos state.blocks
        then landTetramino state
        else {state with activeTetramino = {state.activeTetramino with coords = potentialTetraminoPos }}

let commandHandler command state =    
    match command with
        | Tick -> gameTick state
        | _ -> state
    //    | MoveLeft  -> move (-1) 0 state
    //    | MoveRight -> move (+1) 0 state
    //    | ShiftDown -> move 0 (-1) state
