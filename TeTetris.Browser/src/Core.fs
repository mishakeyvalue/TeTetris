module TeTetris.Core

// -- Utils --

module Seq = 
  let repeat items = 
    seq { while true do yield! items }

let (|SeqEmpty|SeqCons|) (xs: 'a seq) = //'
  if Seq.isEmpty xs then SeqEmpty
  else SeqCons(Seq.head xs, Seq.skip 1 xs)

let deattachHead = function
    | SeqCons (x, xs) -> x, xs
// -- --

let WorldWidth = 10
let WorldHeight = 22

type Block = {
    x: int
    y: int
}

type TetraminoShape =
    | Cube
    | Palka

type Tetramino = {
    shape: TetraminoShape
    coords: Block list
}

type State = {
    tetraminoQueue: TetraminoShape seq
    activeTetramino: Tetramino
    blocks: Block list
}

type Game =
    | NotStarted
    | InProgress of State
    | End
let startPoint = {x=WorldWidth-5;y=WorldHeight-5}

let initTetramino = function
    | Cube ->
        { shape=Cube;
          coords=
            [ startPoint
              { startPoint with x = startPoint.x+1 }
              { startPoint with y = startPoint.y+1 }
              { startPoint with y = startPoint.y+1; x = startPoint.x + 1 }
            ]
        }
        
    | Palka ->
        { shape=Palka;
          coords=
            [ startPoint
              { startPoint with y = startPoint.y+1 }
              { startPoint with y = startPoint.y+2 }
              { startPoint with y = startPoint.y+3 }
            ]
        }

let emptyState (x::xs) = { tetraminoQueue= Seq.repeat xs; activeTetramino=initTetramino x; blocks=[{x=0;y=0}] }
let moveBlock xo yo bl = { bl with x = bl.x+xo;y = bl.y+yo }
let moveTetramino xo yo t =
    
    { t with coords = t.coords |> List.map (moveBlock xo yo) }
    


let allPairs xs ys = 
    [ for x in xs do
      for y in ys do
        yield (x, y)
    ]

let isTetraminoConflict t bls =
    let isSetOnAnotherBlock t bls = 
        allPairs t.coords bls
        |> List.exists (fun (b1, b2) -> b1.x = b2.x && b1.y = b2.y)
    let isSetOnTheGround t = t.coords |> List.exists (fun b -> b.y < 0)

    isSetOnTheGround t || isSetOnAnotherBlock t bls

let destroyFullRows bs =
    let rows = bs |>  List.groupBy (fun b -> b.y)
    let resultRows = rows |> List.filter (fun (_, bs') -> bs'.Length < WorldWidth)          
    let destroyedOffset = rows.Length - resultRows.Length
    resultRows |> List.collect (fun (_, bs) -> bs) |> List.map (moveBlock 0 -destroyedOffset)

let gameTick state =

    let processTeraminoSet state =
        let (newActiveTetramino, tQueue) = deattachHead state.tetraminoQueue
        let newBlocks = 
            state.activeTetramino.coords 
            |> List.append state.blocks

        { state with 
            activeTetramino = initTetramino newActiveTetramino
            tetraminoQueue = tQueue
            blocks = newBlocks
        }
    
    let tetraminoTick = moveTetramino 0 -1 

    let checkedState = { state with blocks = destroyFullRows state.blocks}
    let movedTetramino = tetraminoTick state.activeTetramino
    if isTetraminoConflict movedTetramino state.blocks
        then processTeraminoSet checkedState
        else { checkedState with activeTetramino = movedTetramino }

type GameCommand = 
    | Tick
    | MoveRight
    | MoveLeft
    | ShiftDown

let move xo yo state =
    let movedTetramino = moveTetramino xo yo state.activeTetramino
    if isTetraminoConflict movedTetramino state.blocks 
        || movedTetramino.coords |> List.exists (fun b -> b.x < 0 || b.x >= WorldWidth)
        then state
        else { state with activeTetramino = movedTetramino }

let commandHandler command state =
    match command with
        | Tick -> gameTick state
        | MoveLeft  -> move (-1) 0 state
        | MoveRight -> move (+1) 0 state
        | ShiftDown -> move 0 (-1) state
