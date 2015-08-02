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

let allPositionOrderings = List.permutations [1..9]

let positionOrderingStrategy positions board _ =
    let groups = groupPositionsByCell board
    let emptyPositions = getPositions Empty groups
    positions |> List.find (fun p -> Set.contains p emptyPositions)

let allStrategiesAndOrders : seq<Strategy * int list> =
    allPositionOrderings |> Seq.map (fun order -> positionOrderingStrategy order, order)

let allGamesAndOrders =
    allStrategiesAndOrders |> Seq.map (fun (strategy, order) ->
        let game = makeGame strategy UnbeatableComputer.strategy
        let draw = (fun _ -> ())
        play draw game, order
    )

let lostGamesStrategies =
    allGamesAndOrders
    |> Seq.filter (fun (game, _) -> game.status = Complete (Winner X))

lostGamesStrategies
|> Seq.truncate 3
|> Seq.iter (fun (game, order) ->
    Draw.drawGame game
    printfn "%A" order
    printfn "%s" "")