#load "Result.fs"
#load "Game.fs"
#load "Draw.fs"
#load "Human.fs"
#load "UnbeatableComputer.fs"

open Game

module List = 
    // From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    let rec insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let stopwatch = System.Diagnostics.Stopwatch ()
stopwatch.Start ()

let allPositionOrderings = List.permutations [1..9]

let positionOrderingStrategy positions board _ =
    let groups = groupPositionsByCell board
    let emptyPositions = getPositions Empty groups
    positions |> List.find (fun p -> Set.contains p emptyPositions)

let allStrategies =
    allPositionOrderings |> Seq.map (fun order -> positionOrderingStrategy order)

let allGameHistories =
    allStrategies
    |> Seq.toArray
    |> Array.Parallel.map (fun strategy ->
        let game = makeGame strategy UnbeatableComputer.strategy
        let draw = (fun _ -> ())
        play draw game)

let outcome gameHistory =
    let (game, _, _) = gameHistory |> List.rev |> List.head
    match game.status with
    | Complete result -> result
    | _ -> raise (System.Exception "Game did not complete.")

let groupedByOutcome =
    allGameHistories |> Seq.groupBy outcome |> Map.ofSeq

let printMoves history = 
    history
    |> List.map (fun (game, symbol, position) -> (symbolString symbol) + " " + position.ToString ())
    |> String.concat "  ->  "
    |> printfn "%s"

let gamesWithResult result =
    match Map.tryFind result groupedByOutcome with
    | Some histories -> histories
    | None -> seq []

let lostGames = gamesWithResult (Winner X)
let drawnGames = gamesWithResult Draw
let wonGames = gamesWithResult (Winner O)

lostGames
|> Seq.iter printMoves

printfn "%s" ("Games won: " + (Seq.length wonGames |> string))
printfn "%s" ("Games drawn: " + (Seq.length drawnGames |> string))
printfn "%s" ("Games lost: " + (Seq.length lostGames |> string))
printfn "%s" ("Minutes elapsed: " + (stopwatch.Elapsed.TotalMinutes |> string))