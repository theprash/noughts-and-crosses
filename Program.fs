open System

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

let startingBoard : Board = List.init 9 (fun _ -> Empty)

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

let drawBoard board =
    let cellToString =
        function
        | _, Full X -> "X"
        | _, Full O -> "O"
        | position, Empty -> position.ToString()

    let characters = List.zip [1 .. 9] board |> List.map cellToString

    let displayString =
        match characters with
        | [a; b; c; d; e; f; g; h; i] ->
            [[a; b; c];
             [d; e; f];
             [g; h; i]]
            |> List.map (String.concat " ")
            |> List.collect (fun line -> [line + Environment.NewLine])
            |> String.concat ""
        | _ -> ""

    printfn "%s" displayString

let drawGame game =
    drawBoard game.board
    game

let makeGame playerXInput playerOInput =
    let playerX = {symbol = X; strategy = playerXInput}
    let playerO = {symbol = O; strategy = playerOInput}
    {
        board = startingBoard
        players = playerX, playerO
        nextPlayer = playerX
    }

[<EntryPoint>]
let main argv = 
    let dummyStrategy =
        let dummyMove =
            (fun _ -> 1, X)
        (fun _ -> dummyMove)
    let game = makeGame dummyStrategy dummyStrategy
    let (p1, p2) = game.players
    game |> drawGame
    |> playMove p1 1 |> drawGame
    |> playMove p2 2 |> drawGame
    |> playMove p1 9 |> drawGame
    |> ignore
    Console.ReadLine () |> ignore
    0