module day00_part00

open System.IO
open Utilities

let path = "day00/day00_input.txt"
let inputLines = GetLinesFromFile(path) |> List.ofArray |> List.map (int)

let execute =
    0