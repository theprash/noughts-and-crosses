module Human
open Game
open System

let getPositionFromConsole symbol =
    let rec loop () =
        try
            Console.ReadLine () |> int
        with :? FormatException ->
            Draw.printSpaced ("-- Invalid number. Try again:")
            loop ()
    Draw.printSpaced ("Player " + symbolString symbol + ", Enter your move:")
    loop ()

let strategy _ = getPositionFromConsole