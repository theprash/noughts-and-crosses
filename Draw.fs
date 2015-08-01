module Draw
open Game
open Result
open System

let renderBoard board =
    let cellToString =
        function
        | _, Full s -> symbolString s
        | position, Empty -> position.ToString ()

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

let printSpaced s =
    printfn "%s" ""
    printfn "%s" s
    
let drawGame game =
    game.board |> renderBoard |> printSpaced
    game

let drawGameResult gameResult =
    match gameResult with
    | Success g ->
        System.Console.Clear ()
        drawGame g
    | Failure (failure, symbol, position, game) ->
        let message =
            match failure with
            | PositionFull ->
                "-- Position " + position.ToString () + " is full!"
            | PositionNotInRange ->
                "-- Position " + position.ToString () + " is not in the range 1-9!"
        printSpaced message
        game