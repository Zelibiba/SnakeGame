module SnakeGame =
    open System

    type Coords = int*int
    type Game =
        {
        Walls: Coords array
        Snake: Coords array
        Direction: Coords
        Apple: Coords
        Score: int
        CreateApple: Coords array -> Coords
        IncScore: int -> int
        Finalize: unit -> unit
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
    let drawBox color (top, left) (height, width) =
        let bottom = top + height - 1
        let right = left + width - 1
        drawChar color '\u2554' (top, left)
        drawChar color '\u2557' (top, right)
        drawChar color '\u255A' (bottom, left)
        drawChar color '\u255D' (bottom, right)
        Array.allPairs [| top; bottom |] [| (left+1) .. (right-1) |]
        |> Array.iter (drawChar color '\u2550')
        Array.allPairs [| (top+1) .. (bottom-1) |] [| left; right |]
        |> Array.iter (drawChar color '\u2551')

        let spaces = String.replicate (width-2) " "
        for r = top+1 to bottom-1 do
            Console.SetCursorPosition (left+1, r)
            Console.Write(spaces)

    let printScore rows columns score = 
        Console.ForegroundColor <- ConsoleColor.White
        Console.SetCursorPosition (columns+3, rows/5)
        let score' = score + 1
        printf "Score: %d" score'
        score'
    let printFinal rows columns () = 
        let centerR = 1+rows/2
        let centerC = 1+columns/2
        drawBox ConsoleColor.White (centerR-2, centerC-8) (5, 16)
        Console.SetCursorPosition (centerC-6, centerR)
        Console.Write("End of game!")
        drawChar ConsoleColor.White ' ' (rows+3, 0)

    module SnakeDrawing =
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
        let drawHead (snake: Coords array) =
            let shape = (getSnakeDirection snake[1] snake[0],
                         getSnakeDirection snake[1] snake[2])
            let char = match shape with
                       | MATCH (Up, Down) _ -> '\u2502'
                       | MATCH (Left, Right) _ -> '\u2500'
                       | MATCH (Up, Right) _ -> '\u2514'
                       | MATCH (Up, Left) _ -> '\u2518'
                       | MATCH (Down, Right) _ -> '\u250C'
                       | MATCH (Down, Left) _ -> '\u2510'
                       | _ -> '?'
            drawSymbol SnakeHead snake[0]
            drawSymbol (SnakeTail char) snake[1]

    
    let initWalls rows columns =
        drawBox ConsoleColor.Red (0, 0) (rows+2, columns+2)
        let wallH = Array.allPairs [| 0; rows+1 |] [| 1.. columns |]
        let wallV = Array.allPairs [| 1.. rows |] [| 0; columns+1 |]
        Array.append wallH wallV
    let initSnake row column = 
        let snake = [| (row, column); (row+1, column); (row+2, column) |]
        SnakeDrawing.drawHead snake
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
        Console.SetWindowSize (columns+15, rows+4)
        Console.SetBufferSize (columns+15, rows+4)
        Console.CursorVisible <- false
        let createApple = initAppleFactory rows columns
        let snake = initSnake (rows/2) (columns/2)
        let incScore = printScore rows columns
        let game =
            {
            Walls = initWalls rows columns
            Snake = snake
            Direction = (-1, 0)
            Apple = createApple snake
            Score = incScore -1
            CreateApple = createApple
            IncScore = incScore
            Finalize = printFinal rows columns
            }
        Ok game


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
    let eatApple snake apple =
        let snake' = Array.insertAt 0 apple snake
        SnakeDrawing.drawHead snake'
        snake'
    let move (snake: Coords array) coordsAhead =
        drawSymbol Space snake[snake.Length-1]
        for i = snake.Length-1 downto 1 do
            snake[i] <- snake[i-1]
        snake[0] <- coordsAhead
        SnakeDrawing.drawHead snake
        snake
    let run (game: Game): Result<Game,Game> =
        let { Snake = snake
              Direction = direction
              Apple = apple
              Score = score } = game
        Threading.Thread.Sleep(150)
        let (dirR, dirC) = match getDirection direction with
                            | Some direction' -> direction'
                            | None -> direction
        let (headR, headC) = snake[0]
        let coordsAhead = (headR + dirR, headC + dirC)
        if coordsAhead = apple then
            Ok {game with Snake = eatApple snake apple
                          Direction = (dirR, dirC)
                          Apple = game.CreateApple snake
                          Score = game.IncScore score }
        elif Array.contains coordsAhead game.Walls then Error game
        elif Array.contains coordsAhead snake then Error game
        else
            Ok {game with Snake = move snake coordsAhead
                          Direction = (dirR, dirC) }
    

    let rec play (gameState: Result<Game, Game>) =
        match gameState with
            | Error game -> game.Finalize()
            | Ok game -> play (run game)


open System
[<EntryPoint>]
let main _ =
    let rows, columns = 20, 20

    SnakeGame.init rows columns
    |> SnakeGame.play
    |> ignore

    Console.ResetColor()
    Console.ReadKey(true) |> ignore
    0