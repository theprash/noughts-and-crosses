open Game
open Result

[<EntryPoint>]
let main argv = 
    let game = makeGame Human.strategy Human.strategy
    game |> play Draw.drawGameResult |> ignore
    System.Console.ReadLine () |> ignore
    0