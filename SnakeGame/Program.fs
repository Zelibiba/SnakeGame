open System

type Coords = int*int
type Game =
    {
    Walls: Coords array
    CreateApple: Coords array -> Coords
    mutable Snake: Coords array
    mutable Direction: Coords
    mutable Apple: Coords
    }
type GameState = 
    | Running of Game
    | End


type Symbol =
    | Space
    | SnakeHead
    | SnakeTail of char
    | Apple
let drawChar color char (row, column) =
    Console.ForegroundColor <- color
    Console.SetCursorPosition (column, row)
    Console.Write(char: char)
let drawSymbol symbol =
    match symbol with
        | Space     -> drawChar ConsoleColor.White  ' '
        | SnakeHead -> drawChar ConsoleColor.Yellow '\u2588'
        | SnakeTail char -> drawChar ConsoleColor.Yellow char
        | Apple     -> drawChar ConsoleColor.Green  '@'

type SnakeBodyShape =
    | Up
    | Right
    | Down
    | Left
let getSnakeDirection (midR, midC) (edgeR, edgeC) =
    if midR = edgeR then
        if edgeC > midC
            then Right
            else Left
    elif midC = edgeC then
        if edgeR > midR
            then Down
            else Up
    else failwith "Snake body is cutted!"
let (|MATCH|_|) sample (shape1, shape2) =
    if ((shape1, shape2) = sample) || ((shape2, shape1) = sample)
        then Some sample
        else None
let drawSnakeHead (snake: Coords array) =
    let shape = (getSnakeDirection snake[1] snake[0],
                 getSnakeDirection snake[1] snake[2])
    let char = match shape with
               | MATCH (Up, Down) _ -> '\u2502'
               | MATCH (Left, Right) _ -> '\u2500'
               | MATCH (Up, Right) _ -> '\u2514'
               | MATCH (Up, Left) _ -> '\u2518'
               | MATCH (Down, Right) _ -> '\u250C'
               | MATCH (Down, Left) _ -> '\u2510'
               | _ -> 'x'
    drawSymbol SnakeHead snake[0]
    drawSymbol (SnakeTail char) snake[1]


let initWalls rows columns =
    drawChar ConsoleColor.Red '\u2554' (0, 0)
    drawChar ConsoleColor.Red '\u2557' (0, columns+1)
    drawChar ConsoleColor.Red '\u255A' (rows+1, 0)
    drawChar ConsoleColor.Red '\u255D' (rows+1, columns+1)
    let wallH = Array.allPairs [| 0; rows+1 |] [| 1.. columns |]
    wallH |> Array.iter (drawChar ConsoleColor.Red '\u2550')
    let wallV = Array.allPairs [| 1.. rows |] [| 0; columns+1 |]
    wallV |> Array.iter (drawChar ConsoleColor.Red '\u2551')
    Array.append wallH wallV
let initSnake row column = 
    let snake = [| (row, column); (row+1, column); (row+2, column) |]
    drawSnakeHead snake
    snake
let initAppleFactory rows columns =
    let createApple appleSeq snake =
        let apple =
            appleSeq
            |> Seq.skipWhile (fun coords -> Array.contains coords snake)
            |> Seq.head
        drawSymbol Apple apple
        apple
    let random = Random()
    Seq.initInfinite (fun _ -> (random.Next(1, rows), random.Next(1, columns)))
    |> createApple
let init rows columns =
    Console.SetWindowSize (columns+50, rows+20)
    Console.SetBufferSize (columns+50, rows+20)
    Console.CursorVisible <- false
    let Game =
        {
        Walls = initWalls rows columns
        CreateApple = initAppleFactory rows columns
        Snake = initSnake (rows/2) (columns/2)
        Direction = (-1, 0)
        Apple = (0, 0)
        }
    Game.Apple <- Game.CreateApple Game.Snake 
    Game


let getDirection (dirR, dirC) =
    seq { while Console.KeyAvailable do
            match Console.ReadKey(false).Key with
                | ConsoleKey.UpArrow -> Some (-1, 0)
                | ConsoleKey.RightArrow -> Some (0, 1)
                | ConsoleKey.DownArrow -> Some (1, 0)
                | ConsoleKey.LeftArrow -> Some (0, -1)
                | _ -> None }
    |> Seq.rev
    |> Seq.skipWhile (function
                        | None -> true
                        | Some (r, c) -> (r = dirR) || (c = dirC) )
    |> Seq.tryHead
    |> function
        | Some value -> value
        | None -> None
let eatApple (snake: Coords array) apple =
    let snake' = Array.insertAt 0 apple snake
    drawSnakeHead snake'
    snake'
let move (snake: Coords array) coordsAhead =
    drawSymbol Space snake[snake.Length-1]
    for i = snake.Length-1 downto 1 do
        snake[i] <- snake[i-1]
    snake[0] <- coordsAhead
    drawSnakeHead snake
    snake
let run (game: Game): Result<Game,unit> =
    Threading.Thread.Sleep(150)
    match getDirection game.Direction with
        | Some direction -> game.Direction <- direction
        | None -> ()
    let (dirR, dirC) = game.Direction
    let (headR, headC) = game.Snake[0]
    let coordsAhead = (headR + dirR, headC + dirC)
    if coordsAhead = game.Apple then
        game.Snake <- eatApple game.Snake game.Apple
        game.Apple <- game.CreateApple game.Snake
        Ok game
    elif Array.contains coordsAhead game.Walls then
        Error()
    elif Array.contains coordsAhead game.Snake then
        Error()
    else
        game.Snake <- move game.Snake coordsAhead
        Ok game
        


let rec play (gameState: Result<Game, unit>) =
    match gameState with
        | Error _ -> ()
        | Ok game -> play (run game)


[<EntryPoint>]
let main _ =
    let rows, columns = 20, 20
    let game = init rows columns

    play (run game) |> ignore

    drawChar ConsoleColor.White ' ' (rows+3, 0)
    Console.ResetColor()
    0