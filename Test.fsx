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

let allGames =
    allStrategies
    |> Seq.map (fun strategy ->
        let game = makeGame strategy UnbeatableComputer.strategy
        let draw = (fun _ -> ())
        play draw game)

let lostGames =
    allGames
    |> Seq.filter (fun gameHistory ->
        let (game, _, _) = gameHistory |> List.rev |> List.head
        game.status = Complete (Winner X))
    |> Seq.truncate 3
    |> Seq.toList

let printMoves history = 
    history
    |> List.map (fun (game, symbol, position) -> (symbolString symbol) + " " + position.ToString ())
    |> String.concat "  ->  "
    |> printfn "%s"

lostGames
|> List.iter printMoves

printfn "%s" ("Failing cases: " + (List.length lostGames).ToString ())
printfn "%s" ("Minutes elapsed: " + stopwatch.Elapsed.TotalMinutes.ToString ())