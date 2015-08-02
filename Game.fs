/// Contains the game logic. No IO is performed.
module Game
open Result

type Symbol = X | O

type Cell = Full of Symbol | Empty

type Board = Cell list

type Position = int

type Strategy = Board -> Symbol -> Position

type [<NoEquality>] [<NoComparison>]
    Player =
    {
        symbol: Symbol
        strategy: Strategy
    }

type Outcome =
    | Draw
    | Winner of Symbol

type Status =
    | InProgress
    | Complete of Outcome

type [<NoEquality>] [<NoComparison>]
    Game =
    {
        board: Board
        players: Player * Player
        nextPlayer: Player
        status: Status
    }

type Failure =
    | PositionFull
    | PositionNotInRange

let symbolString = function X -> "X" | O -> "O"

/// A line is a set of 3 positions that would result in a win
let lines =
    [
        // Horizontals
        set [1; 2; 3]
        set [4; 5; 6]
        set [7; 8; 9]

        // Verticals
        set [1; 4; 7]
        set [2; 5; 8]
        set [3; 6; 9]

        // Diagonals
        set [1; 5; 9]
        set [3; 5; 7]
    ]

let groupPositionsByCell board =
    board
    |> Seq.zip [1 .. 9]
    |> Seq.groupBy snd
    |> Seq.map (fun (value, positionAndCells) ->
        let positions = positionAndCells |> Seq.map fst
        value, Set.ofSeq positions)
    |> Map.ofSeq

let getPositions cell groups =
    defaultArg
        (Map.tryFind cell groups)
        Set.empty

let updateListAt index value list =
    List.mapi (fun i x -> if i = index then value else x) list

let updateBoard symbol position board =
    updateListAt (position - 1) (Full symbol) board

let getCell position board = List.nth board (position - 1)

let validateMove position game =
    match game with
    | _ when not (1 <= position && position <= 9) ->
        Failure PositionNotInRange
    | _ when getCell position game.board <> Empty ->
        Failure PositionFull
    | _ ->
        Success ()

let swapNextPlayer game =
    let (p1, p2) = game.players
    {game with
        nextPlayer = if game.nextPlayer.symbol = p1.symbol then p2 else p1}

let boardFull = List.exists ((=) Empty) >> not

let getWinner board =
    let groups = groupPositionsByCell board
    let containsLine positions =
        Seq.exists (Set.isSuperset positions) lines

    let XPositions = getPositions (Full X) groups
    let OPositions = getPositions (Full O) groups

    if containsLine XPositions then
        Some X
    else if containsLine OPositions then
        Some O
    else
        None

let updateStatus game =
    {game with
        status =
            match getWinner game.board with
            | Some X -> Complete (Winner X)
            | Some Y -> Complete (Winner Y)
            | _ when boardFull game.board -> Complete Draw
            | None -> InProgress}

let tryPlayMove (position:Position) symbol game =
    match validateMove position game with
    | Success _ ->
        {game with board = updateBoard symbol position game.board}
        |> swapNextPlayer
        |> updateStatus
        |> Success
    | Failure failure -> Failure (failure, position, game)

let getGameFromResult = function
    | Success g -> g
    | Failure (_, _, g) -> g

let play drawGameResult game =
    let rec loop game acc =
        let currentPlayer = game.nextPlayer
        let symbol = currentPlayer.symbol
        let position = currentPlayer.strategy game.board symbol
        let gameResult = tryPlayMove position symbol game
        drawGameResult gameResult
        let nextGame = getGameFromResult gameResult
        let nextAcc = (nextGame, symbol, position) :: acc
        match nextGame.status with
        | InProgress ->
            loop nextGame nextAcc
        | Complete _ ->
            List.rev nextAcc
    drawGameResult (Success game)
    loop game []

let makeGame playerXStrategy playerOStrategy =
    let startingBoard = List.init 9 (fun _ -> Empty)
    let playerX = {symbol = X; strategy = playerXStrategy}
    let playerO = {symbol = O; strategy = playerOStrategy}
    {
        board = startingBoard
        players = playerX, playerO
        nextPlayer = playerX
        status = InProgress
    }