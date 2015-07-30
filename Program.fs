open System

type Symbol = X | O

type PlayerInput =
    | Human
    | Computer

type Player = Symbol * PlayerInput

type Cell = Full of Symbol | Empty

type Board = Cell list

type Game =
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
    let (symbol, _) = player

    let nextPlayer =
        let (p1, p2) = game.players
        if player = p1 then p2 else p1
    {game with
        board = updateBoard symbol position game.board
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
    let playerX = X, playerXInput
    let playerO = O, playerOInput
    {
        board = startingBoard
        players = playerX, playerO
        nextPlayer = playerX
    }

[<EntryPoint>]
let main argv = 
    let game = makeGame Human Human
    let (p1, p2) = game.players
    game |> drawGame
    |> playMove p1 1 |> drawGame
    |> playMove p2 2 |> drawGame
    |> playMove p1 9 |> drawGame
    |> ignore
    Console.ReadLine () |> ignore
    0