module TeTetris.Game.Types

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

type GameCommand = 
    | Tick
    | MoveRight
    | MoveLeft
    | ShiftDown