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
    | SnakeTail
    | Apple
let drawChar color char (row, column) =
    Console.ForegroundColor <- color
    Console.SetCursorPosition (column, row)
    Console.Write(char: char)
let drawSymbol symbol =
    match symbol with
        | Space     -> drawChar ConsoleColor.White  ' '
        | SnakeHead -> drawChar ConsoleColor.Yellow 'O'
        | SnakeTail -> drawChar ConsoleColor.Yellow 'o'
        | Apple     -> drawChar ConsoleColor.Green  '@'


let initWalls rows columns =
    drawChar ConsoleColor.Red '\u2554' (0, 0)
    drawChar ConsoleColor.Red '\u2557' (0, columns+1)
    drawChar ConsoleColor.Red '\u255A' (rows+1, 0)
    drawChar ConsoleColor.Red '\u255D' (rows+1, columns+1)
    let walls = Array.allPairs [| 0; rows+1 |] [| 1.. columns |]
    walls |> Array.iter (drawChar ConsoleColor.Red '\u2550')
    let wall = Array.allPairs [| 1.. rows |] [| 0; columns+1 |]
    wall |> Array.iter (drawChar ConsoleColor.Red '\u2551')
    Array.append walls wall
let initSnake row column = 
    let snake = [| (row, column); (row+1, column) |]
    drawSymbol SnakeHead snake[0] 
    drawSymbol SnakeTail snake[1]
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


let getCommand() =
    match Console.ReadKey(false).Key with
        | ConsoleKey.UpArrow -> (-1, 0)
        | ConsoleKey.RightArrow -> (0, 1)
        | ConsoleKey.DownArrow -> (1, 0)
        | ConsoleKey.LeftArrow -> (0, -1)
        //| _ -> None
let eatApple (snake: Coords array) apple=
    drawSymbol SnakeTail snake[0]
    let snake' = Array.insertAt 0 apple snake
    drawSymbol SnakeHead snake'[0]
    snake'
let move (snake: Coords array) coordsAhead =
    drawSymbol Space snake[snake.Length-1]
    for i = snake.Length-1 downto 1 do
        snake[i] <- snake[i-1]
    drawSymbol SnakeTail snake[0]
    snake[0] <- coordsAhead
    drawSymbol SnakeHead snake[0]
    snake
let run (game: Game): Result<Game,unit> =
    let (dirR, dirC) = getCommand()
    let (headR, headC) = game.Snake[0]
    let coordsAhead = (headR + dirR, headC + dirC)
    if coordsAhead = game.Apple then
        game.Snake <- eatApple game.Snake game.Apple
        game.Apple <- game.CreateApple game.Snake
        Ok game
    elif Array.contains coordsAhead game.Walls then
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
    let rows, columns = 30, 30
    let game = init rows columns

    play (Ok game) |> ignore

    drawChar ConsoleColor.White ' ' (rows+2, 0)
    Console.ResetColor()
    0