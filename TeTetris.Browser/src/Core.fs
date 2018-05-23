module TeTetris.Core

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
    tetraminoQueue: TetraminoShape list
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

let emptyState (x::xs) = { tetraminoQueue=xs; activeTetramino=initTetramino x; blocks=[{x=0;y=0}] }

let moveTetramino t =
    let moveBlock bl = { bl with y = bl.y-1 }
    { t with coords = t.coords |> List.map  moveBlock }


let allPairs xs ys = 
    [ for x in xs do
      for y in ys do
        yield (x, y)
    ]

let isTetraminoSet t bls =
    let isSetOnAnotherBlock t bls = 
        allPairs t.coords bls
        |> List.exists (fun (b1, b2) -> b1.x = b2.x && b1.y = b2.y)
    let isSetOnTheGround t = t.coords |> List.exists (fun b -> b.y = 0)

    isSetOnTheGround t || isSetOnAnotherBlock t bls

let gameTick state =
    let movedTetramino = moveTetramino state.activeTetramino
    if isTetraminoSet movedTetramino state.blocks
        then { state with activeTetramino = movedTetramino }
        else { state with activeTetramino = movedTetramino }
