open Game

let renderBoard board =
    let cellToString =
        function
        | _, Full X -> "X"
        | _, Full O -> "O"
        | position, Empty -> position.ToString()

    let characters = List.zip [1 .. 9] board |> List.map cellToString

    match characters with
    | [a; b; c; d; e; f; g; h; i] ->
        [[a; b; c];
         [d; e; f];
         [g; h; i]]
        |> List.map (String.concat " ")
        |> List.collect (fun line -> [line + "\n"])
        |> String.concat ""
    | _ -> ""

let drawGame game =
    game.board |> renderBoard |> printfn "%s"
    game

let makeGame playerXInput playerOInput =
    let startingBoard = List.init 9 (fun _ -> Empty)
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
    System.Console.ReadLine () |> ignore
    0