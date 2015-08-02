module UnbeatableComputer
open Game

(*
The computer strategy will be:
 - If I have a winning move, then play it
 - If you have a winning move, then play there to block it
 - If I have a move that creates two possible winning moves, then play there
 - If you have a move that creates two possible winning moves, then play there to block it
 - Otherwise play in the lowest available position (deterministic behaviour may simplify testing)

This should avoid a loss, while making a reasonable attempt to win.
*)

let winningMove board symbol groups =
    let fullPositions = getPositions (Full symbol) groups
    let emptyPositions = getPositions Empty groups
    lines
    |> List.map (fun line -> Set.difference line fullPositions) // remove the full positions
    |> List.filter (Set.count >> ((=) 1)) // keep those with one position left
    |> List.filter (Set.isSuperset emptyPositions) // keep those that are for empty positions
    |> List.collect Set.toList
    |> List.sort
    |> List.tryPick Some

let myWinningMove board mySymbol groups = winningMove board mySymbol groups

let yourWinningMove board mySymbol groups =
    let yourSymbol = match mySymbol with X -> O | O -> X
    winningMove board yourSymbol groups

let lowestAvailable board mySymbol groups =
    getPositions Empty groups |> Set.minElement |> Some

let bestMoves board mySymbol groups =
    seq {
        yield myWinningMove board mySymbol groups
        yield yourWinningMove board mySymbol groups
        yield lowestAvailable board mySymbol groups
    }
    |> Seq.choose id

let strategy board mySymbol =
    let groups = groupPositionsByCell board
    bestMoves board mySymbol groups
    |> Seq.pick Some