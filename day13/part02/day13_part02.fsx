open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let path = "day13_input.txt"
//let path = "test_input.txt"

let instructionsParts = 
    File.ReadLines(__SOURCE_DIRECTORY__ + @"../../" + path) |> Seq.toList

let parts = instructionsParts |> List.splitAt(instructionsParts |> List.findIndex(fun l -> l = ""))
let coordinates = (fst parts) |> List.map(fun c -> [|c.Split(',').[0] |> int; c.Split(',').[1] |> int|])
let folding = (snd parts).Tail |> List.map(fun i -> (i.Split('=').[0].Substring(i.Split('=').[0].Length - 1, 1), i.Split('=').[1] |> int))

let createBoard(coords: int[] list) =
    let maxX = ((coords |> List.sortByDescending(fun c -> c.[1])) |> List.head).[1] + 1
    let maxY = ((coords |> List.sortByDescending(fun c -> c.[0])) |> List.head).[0] + 1
    let board = Array2D.create maxX maxY "."
    coords |> List.iter(fun c -> board[c.[1], c.[0]] <- "#")
    board

let foldBoard(board: string[,], fold: string * int) =
    let newBoard = 
        match (fst fold) with
        | "y" -> 
            Array2D.create (board.GetLength(0) / 2) (board.GetLength 1) "."
        | "x" -> 
            Array2D.create (board.GetLength 0) (board.GetLength(1) / 2) "."
        | _ -> Array2D.create 0 0 "."
    for row in [0..newBoard.GetLength(0) - 1] do
        for col in [0..newBoard.GetLength(1) - 1] do
            let mirrored = 
                match (fst fold) with
                | "y" -> board[board.GetLength(0) - 1 - row, col]
                | "x" -> board[row, board.GetLength(1) - 1 - col]
                | _ -> ""
            newBoard[row, col] <- if mirrored = "#" then mirrored else board[row, col]
    newBoard

let toJagged<'a> (arr: 'a[,]) : 'a[][] =
    [|for x in 0..Array2D.length1 arr - 1 do
        yield [| for y in 0 ..Array2D.length2 arr - 1 -> arr.[x, y] |]|]

let board = createBoard coordinates
let rec fold(theboard: string[,], folds: (string * int) list) =
    match folds with
    | [] -> theboard
    | x::xs -> 
        let foldedBoard = foldBoard(theboard, x)
        fold(foldedBoard, xs)

//let firstFolding = foldBoard(board, folding.Head)
//let tBoard = toJagged firstFolding
//let numberOfDots = (tBoard |> Array.map(fun e -> e |> Array.filter(fun ee -> ee = "#") |> Array.length)) |> Array.sum   
//numberOfDots

let finalBoard = fold(board, folding)
//let newBoard = foldBoard(board, ("y", 7))

//for col in [0..board.GetLength(0) - 1] do
//    for row in [0..board.GetLength(1) - 1] do
//        printf "%s" board[col, row]
//    printfn ""

let tBoard = toJagged finalBoard
//let tBoard = toJagged secondFolding
let numberOfDots = (tBoard |> Array.map(fun e -> e |> Array.filter(fun ee -> ee = "#") |> Array.length)) |> Array.sum   
numberOfDots


for col in [0..finalBoard.GetLength(0) - 1] do
    for row in [0..finalBoard.GetLength(1) - 1] do
        printf "%s" finalBoard[col, row]
    printfn ""