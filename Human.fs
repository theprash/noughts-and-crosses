module Human
open Game
open System

let getPositionFromConsole symbol =
    let rec loop () =
        try
            Console.ReadLine () |> int
        with :? FormatException ->
            printfn "%s" ("-- Invalid number. Try again:")
            loop ()
    printfn "%s" ("Player " + symbolString symbol + ", Enter your move:")
    loop ()

let strategy _ = getPositionFromConsole