module UnbeatableComputer
open Game

(*
The computer strategy will be:
 - If I have a winning move, then play it
 - If you have a winning move, then play there to block it
 - Special case: If I have the centre and you have two opposite corners then play on a side
 - If I have a move that creates two possible winning moves (a "winning setup"), then play there
 - If you have a move that creates two possible winning moves, then play there to block it
 - Otherwise play the first in a hard-coded list of key positions (centre, then corners, then rest)
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

let oppositeCorners = [set [1; 9]; set [3; 7] ]

let specialCaseMove mySymbol groups =
    let myPositions = getPositions (Full mySymbol) groups
    let yourSymbol = other mySymbol
    let yourPositions = getPositions (Full yourSymbol) groups
    let iHaveCenter = myPositions = set [5]
    let youHaveOppositeCorners =
        List.exists ((=) yourPositions) oppositeCorners
    if iHaveCenter && youHaveOppositeCorners then
        Some 2
    else
        None

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

let bestRemainingPosition mySymbol groups =
    let emptyPositions = getPositions Empty groups
    [5; 1; 3; 7; 9; 2; 4; 6; 8]
    |> Seq.filter (fun p -> Set.contains p emptyPositions)
    |> Seq.tryPick Some

let bestMoves board mySymbol groups =
    seq {
        yield myWinningMove mySymbol groups
        yield yourWinningMove mySymbol groups
        yield specialCaseMove mySymbol groups
        yield myWinningSetup board mySymbol groups
        yield yourWinningSetup board mySymbol groups
        yield bestRemainingPosition mySymbol groups
    }
    |> Seq.choose id

let strategy board mySymbol =
    let groups = groupPositionsByCell board
    bestMoves board mySymbol groups
    |> Seq.pick Some