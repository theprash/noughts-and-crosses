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

let printSpaced s =
    printfn "%s" ""
    printfn "%s" s
    

let drawGame game =
    game.board |> renderBoard |> printSpaced
    game

let drawGameResult gameResult =
    match gameResult with
    | Success g ->
        drawGame g
    | Failure (failure, symbol, position, game) ->
        let message =
            match failure with
            | PositionFull ->
                "-- Position " + position.ToString() + " is full!"
            | PositionNotInRange ->
                "-- Position " + position.ToString() + " is not in the range 1-9!"
        printSpaced message
        game

let getPositionFromConsole symbol =
    let readInput () = Console.ReadLine () |> int
    let rec loop () =
        try
            readInput ()
        with :? FormatException ->
            printSpaced ("-- Invalid number. Try again:")
            loop ()
    printSpaced ("Player " + symbolString symbol + ": Enter your move:")
    loop ()

let humanPlayer symbol =
    let humanStrategy = fun _ -> getPositionFromConsole symbol
    {symbol = symbol; strategy = humanStrategy}

let makeGame p1 p2 =
    let startingBoard = List.init 9 (fun _ -> Empty)
    {
        board = startingBoard
        players = p1, p2
        nextPlayer = p1
    }

[<EntryPoint>]
let main argv = 
    let game = makeGame (humanPlayer X) (humanPlayer O)
    game |> playtoCompletion drawGameResult |> ignore
    Console.ReadLine () |> ignore
    0