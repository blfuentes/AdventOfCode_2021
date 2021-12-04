module day04_part02

open System
open System.IO

let path = "day04_input.txt"
let inputLines = File.ReadAllText(__SOURCE_DIRECTORY__ + @"../../" + path)

let splitStringBySeparator (content: string) (separator: string) =
    let subcontent = content.Split([|separator|], StringSplitOptions.None)
    subcontent

let buildBoard (content: string) =
    let rows = splitStringBySeparator content "\r\n"
    rows |> Array.map (fun r -> r.Split(' ') |> Array.filter(fun r -> r <> "") |> Array.map int)

let getChar ((row, col):int*int,  board:int[][]) =
    board.[row].[col]

let playBoard(value:int, board: int[][]) =
    let maxCol = board.[0].Length - 1
    let maxRow = board.Length - 1
    for r in [0 .. maxRow] do
        for c in [0 .. maxCol] do
            if getChar((r,c), board) = value then board.[r][c] <- -1
    board

let rec columnIndex(positions:int list, tmpB: int[][]) =         
    match tmpB |> Array.forall(fun r -> r.[positions.Head] = -1) with
    | true -> positions.Head
    | false -> if positions.Tail.Length > 0 then columnIndex(positions.Tail, tmpB) else -1

let IsWinningBoard(board: int[][]) =
    (board |> Array.exists(fun r -> r |> Array.forall(fun a -> a = -1))) || columnIndex([0 .. board.[0].Length - 1], board) <> -1

let getBoardScore(board: int[][]) =
    board |> Array.sumBy(fun x -> x|> Array.filter(fun v -> v <> -1) |> Array.sum)

let rec round (values:int list, listOfBoards: int[][] list) =
    ignore(listOfBoards |> List.map (fun b -> playBoard(values.Head, b)))
    match listOfBoards |> List.filter(fun b -> not(IsWinningBoard(b))) with
    | [] -> (listOfBoards.Head, values.Head)
    | x -> round(values.Tail, x)

let execute =
    let inputParts = splitStringBySeparator inputLines "\r\n\r\n" |> Array.toList
    let playingnumbers = inputParts.Head.Split([|','|]) |> Array.map int |> Array.toList
    let boards = inputParts.Tail |> List.map (fun p -> buildBoard p)
    let winningBoard = round(playingnumbers, boards)
    let score = getBoardScore(fst winningBoard)

    (score*snd winningBoard)