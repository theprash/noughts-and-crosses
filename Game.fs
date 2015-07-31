/// Contains the game logic. No IO is performed.
module Game

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

let updateListAt index value list =
    List.mapi (fun i x -> if i = index then value else x) list

let updateBoard symbol position board =
    let cell = Full symbol
    updateListAt (position - 1) cell board

let cellEmpty position board =
    List.nth board (position - 1) = Empty

let playMove player position game =
    let nextPlayer =
        let (p1, p2) = game.players
        if player.symbol = p1.symbol then p2 else p1
    {game with
        board = updateBoard player.symbol position game.board
        nextPlayer = nextPlayer}