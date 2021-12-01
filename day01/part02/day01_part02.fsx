open System.IO

let path = "day01_input.txt"
//let path = "test_input.txt"
let inputLines = File.ReadLines(__SOURCE_DIRECTORY__ + @"../../" + path) |> Seq.map int |> Seq.toList
//inputLines|> List.take 1

let rec getByNumber (num, lista: 'a list) : 'a list = 
    match num <= lista.Length with
    | true -> [(lista |> List.take num |> List.sum)] @ (getByNumber(num, (lista |> List.skip 1)))
    | false -> []

let groupedByThree = getByNumber(3, inputLines)
groupedByThree |> List.pairwise |> List.filter (fun (x,y) -> y > x) |> List.length