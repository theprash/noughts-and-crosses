open Game
open Result
open System

let symbolString = function X -> "X" | O -> "O"

let renderBoard board =
    let cellToString =
        function
        | _, Full s -> symbolString s
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

let drawGameResult gameResult =
    match gameResult with
    | Success g ->
        drawGame g
    | Failure (failure, player, position, game) ->
        let message =
            match failure with
            | PositionFull ->
                "Position " + position.ToString() + " is full"
        printfn "%s" message
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
    |> tryPlayMove p1 1 |> drawGameResult
    |> tryPlayMove p2 2 |> drawGameResult
    |> tryPlayMove p1 9 |> drawGameResult
    |> ignore
    Console.ReadLine () |> ignore
    0