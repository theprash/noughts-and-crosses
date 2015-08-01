module Human
open Game
open System

let getPositionFromConsole symbol =
    let readInput () = Console.ReadLine () |> int
    let rec loop () =
        try
            readInput ()
        with :? FormatException ->
            Draw.printSpaced ("-- Invalid number. Try again:")
            loop ()
    Draw.printSpaced ("Player " + symbolString symbol + ", Enter your move:")
    loop ()

let strategy = fun _ symbol -> getPositionFromConsole symbol