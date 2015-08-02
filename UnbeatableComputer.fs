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

let strategy board symbol =
    let groups = groupPositionsByCell board
    getPositions Empty groups |> Set.minElement