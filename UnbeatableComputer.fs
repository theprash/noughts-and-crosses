module UnbeatableComputer
open Game

(*
The computer strategy will be:
 - If I have a winning move, then play it
 - If you have a winning move, then play there to block it
 - If I have a move that creates two possible winning moves (a "winning setup"), then play there
 - If you have a move that creates two possible winning moves, then play there to block it
 - Otherwise play in the lowest available position (deterministic behaviour may simplify testing)

This should avoid a loss, while making a reasonable attempt to win.
*)

let winningMoves symbol groups =
    let fullPositions = getPositions (Full symbol) groups
    let emptyPositions = getPositions Empty groups
    lines
    |> List.map (fun line -> Set.difference line fullPositions) // remove the full positions
    |> List.filter (Set.count >> ((=) 1)) // keep those with one position left
    |> List.filter (Set.isSuperset emptyPositions) // keep those that are for empty positions
    |> List.collect Set.toList

let winningMove symbol groups =
    winningMoves symbol groups
    |> List.sort
    |> List.tryPick Some

let myWinningMove = winningMove

let other = function X -> O | O -> X

let yourWinningMove mySymbol groups =
    let yourSymbol = other mySymbol
    winningMove yourSymbol groups

let createsTwoWinningMoves board symbol position =
    let groups =
        board
        |> updateBoard symbol position
        |> groupPositionsByCell
    winningMoves symbol groups
    |> List.length
    |> (<) 1

let winningSetup board symbol groups =
    let emptyPositions = getPositions Empty groups
    emptyPositions
    |> Set.toList
    |> List.sort
    |> List.filter (createsTwoWinningMoves board symbol)
    |> List.tryPick Some

let myWinningSetup = winningSetup

let yourWinningSetup board mySymbol groups =
    let yourSymbol = other mySymbol
    winningSetup board yourSymbol groups


let lowestAvailable mySymbol groups =
    getPositions Empty groups |> Set.minElement |> Some

let bestMoves board mySymbol groups =
    seq {
        yield myWinningMove mySymbol groups
        yield yourWinningMove mySymbol groups
        yield myWinningSetup board mySymbol groups
        yield yourWinningSetup board mySymbol groups
        yield lowestAvailable mySymbol groups
    }
    |> Seq.choose id

let strategy board mySymbol =
    let groups = groupPositionsByCell board
    bestMoves board mySymbol groups
    |> Seq.pick Some