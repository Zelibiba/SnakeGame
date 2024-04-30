open System

type Coords = int*int

type Snake =
    {
        Head: Coords
        Tail: Coords array
    }

type Cell =
    | None
    | Wall
    

let show (coords: Coords) (symbol, color) =
    Console.SetCursorPosition coords
    Console.ForegroundColor <- color
    Console.Write(symbol:string)

let showCell cell coords =
    match cell with
    | None -> " ", ConsoleColor.White
    | Wall -> "\u2551", ConsoleColor.Red
    |> show coords

let init rows columns =
    let horisontal = String.replicate columns "\u2550"
    Console.ForegroundColor <- ConsoleColor.Red

    printfn "%s" ("\u2554" + horisontal + "\u2557")
    Array.allPairs [| 0; columns + 1 |] [| 1..rows |]
    |> Array.iter (showCell Wall)
    printfn "%s" ("\n\u255A" + horisontal + "\u255D")



[<EntryPoint>]
let main argv=
    init 20 20

    Console.ResetColor()
    0