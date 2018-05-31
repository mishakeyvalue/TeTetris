module TeTetris.Game.Types

type Point = {
    x: int
    y: int
}
type Block = {
    color: string
}

type TetraminoShape =
    | Cube
    | Palka
    | L
    | J
    | S
    | Z
    | R

type TetraminoCoords = {
    a: Point
    b: Point
    c: Point
    d: Point
}

type Tetramino = {
    shape: TetraminoShape
    block: Block
    coords: TetraminoCoords
}

type State = {
    tetraminoQueue: TetraminoShape seq
    activeTetramino: Tetramino
    blocks: Map<int, Map<int, Block option>>
}

type Game =
    | NotStarted
    | InProgress of State
    | End

type GameCommand = 
    | Tick
    | MoveRight
    | MoveLeft
    | ShiftDown
    | Rotate