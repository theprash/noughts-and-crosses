/// Contains the game logic. No IO is performed.
module Game
open Result

type Symbol = X | O

type Cell = Full of Symbol | Empty

type Board = Cell list

type Position = int

type Strategy = Game -> Position

and [<NoEquality>] [<NoComparison>]
    Player =
    {
        symbol: Symbol
        strategy: Strategy
    }

and [<NoEquality>] [<NoComparison>]
    Game =
    {
        board: Board
        players: Player * Player
        nextPlayer: Player
    }

type Failures =
    | PositionFull
    | PositionNotInRange

let updateListAt index value list =
    List.mapi (fun i x -> if i = index then value else x) list

let updateBoard symbol position board =
    let cell = Full symbol
    updateListAt (position - 1) cell board

let empty position board =
    List.nth board (position - 1) = Empty

let full position board = empty position board |> not

let validateMove position game =
    match game with
    | _ when not (1 <= position && position <= 9) ->
        Failure PositionNotInRange
    | _ when full position game.board ->
        Failure PositionFull
    | _ ->
        Success ()

let swapPlayers game =
    let (p1, p2) = game.players
    {game with
        nextPlayer = if game.nextPlayer.symbol = p1.symbol then p2 else p1}

let tryPlayMove (position:Position) symbol game =
    match validateMove position game with
    | Success _ ->
        {game with board = updateBoard symbol position game.board}
        |> swapPlayers
        |> Success
    | Failure f -> Failure (f, symbol, position, game)

let playtoCompletion drawGameResult game =
    let rec loop game =
        let currentPlayer = game.nextPlayer
        let position = currentPlayer.strategy game
        let symbol = currentPlayer.symbol
        tryPlayMove position symbol game
        |> drawGameResult
        |> loop
    drawGameResult (Success game) |> ignore
    loop game