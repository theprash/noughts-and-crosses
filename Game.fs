/// Contains the game logic. No IO is performed.
module Game
open Result

type Symbol = X | O

type Cell = Full of Symbol | Empty

type Board = Cell list

type Position = int

type Move = Game -> Position * Symbol

and [<NoEquality>] [<NoComparison>]
    Player =
    {
        symbol: Symbol
        strategy: Game -> Move
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

let updateListAt index value list =
    List.mapi (fun i x -> if i = index then value else x) list

let updateBoard symbol position board =
    let cell = Full symbol
    updateListAt (position - 1) cell board

let empty position board =
    List.nth board (position - 1) = Empty

let full position board = empty position board |> not

let validateMove player position game =
    match game with
    | _ when full position game.board ->
        Failure PositionFull
    | _ ->
        Success ()

let swapPlayers game =
    let (p1, p2) = game.players
    {game with
        nextPlayer = if game.nextPlayer.symbol = p1.symbol then p2 else p1}

let tryPlayMove player (position:Position) game =
    match validateMove player position game with
    | Success _ ->
        {game with board = updateBoard player.symbol position game.board}
        |> swapPlayers
        |> Success
    | Failure f -> Failure (f, player, position, game)