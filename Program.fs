open Game
open Result
open System

let rec chooseGameType () =
    printfn "%s" "Pick a game type."
    printfn "%s" "1. Human vs Human"
    printfn "%s" "2. Human vs Computer"
    printfn "%s" "3. Computer vs Human"
    printfn "%s" "4. Computer vs Computer"
    match Console.ReadLine () with
    | "1" -> Human.strategy, Human.strategy
    | "2" -> Human.strategy, UnbeatableComputer.strategy
    | "3" -> UnbeatableComputer.strategy, Human.strategy
    | "4" -> UnbeatableComputer.strategy, UnbeatableComputer.strategy
    | _ -> chooseGameType ()

[<EntryPoint>]
let main argv =
    let (strategyX, strategyO) = chooseGameType ()
    let game = makeGame strategyX strategyO
    game |> play Draw.drawGameResult |> ignore
    Console.ReadLine () |> ignore
    0