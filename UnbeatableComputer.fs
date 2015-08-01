module UnbeatableComputer
open Game

let strategy board symbol =
    let groups = groupPositionsByCell board
    getPositions Empty groups |> Set.minElement