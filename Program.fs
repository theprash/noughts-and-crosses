open Game
open Result

[<EntryPoint>]
let main argv = 
    let game = makeGame Human.strategy UnbeatableComputer.strategy
    game |> play Draw.drawGameResult |> ignore
    System.Console.ReadLine () |> ignore
    0