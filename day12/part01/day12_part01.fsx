open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

#load @"../../Model/CustomDataTypes.fs"
open CustomDataTypes

//let path = "day12_input.txt"
let path = "test_input00.txt"
//let path = "test_input01.txt"
//let path = "test_input02.txt"

let cavePaths = 
    File.ReadLines(__SOURCE_DIRECTORY__ + @"../../" + path) |> Seq.toList

let getCaveType(value: string) =
    match value with
    | "start" -> CaveType.START
    | "end" -> CaveType.END
    | _ -> if ([97..122] |> List.contains(value.ToCharArray().[0] |> int)) then CaveType.SMALL else CaveType.BIG

let createCave(definition: string) =
    let caveParts = definition.Split('-')
    let connection = { Name = caveParts.[1]; Size = getCaveType(caveParts.[1]); Connections = [] }
    let cave = { Name = caveParts.[0]; Size = getCaveType(caveParts.[0]); Connections = [] }
    (cave, connection)

let rec generateCaves (paths: string list, listOfCaves: Cave list) =
    match paths with
    | [] -> listOfCaves
    | x::xs ->
        let tmpCaves = createCave(x)
        let oldCaveInit = listOfCaves |> List.filter(fun c -> c.Name = (fst tmpCaves).Name)
        let oldCaveEnd = listOfCaves |> List.filter(fun c -> c.Name = (snd tmpCaves).Name)
        if oldCaveInit.IsEmpty then
            if oldCaveEnd.IsEmpty then
                generateCaves (xs, listOfCaves @ 
                [{ Name = (fst tmpCaves).Name; Size = (fst tmpCaves).Size; Connections = [snd tmpCaves] };
                { Name = (snd tmpCaves).Name; Size = (snd tmpCaves).Size; Connections = [] }])                
            else
                generateCaves (xs, listOfCaves @ [{ Name = (fst tmpCaves).Name; Size = (fst tmpCaves).Size; Connections = [snd tmpCaves] }])
        else
            if oldCaveEnd.IsEmpty then
                generateCaves (xs, 
                (listOfCaves |> List.except(oldCaveInit)) @ [{ Name = (fst tmpCaves).Name; Size = (fst tmpCaves).Size; Connections = oldCaveInit.Head.Connections @ [snd tmpCaves] };
                { Name = (snd tmpCaves).Name; Size = (snd tmpCaves).Size; Connections = [] }])
            else
                generateCaves (xs, (listOfCaves |> List.except(oldCaveInit)) @ [{ Name = (fst tmpCaves).Name; Size = (fst tmpCaves).Size; Connections = oldCaveInit.Head.Connections @ [snd tmpCaves] }])

let buildCaveMap (paths: string list) =
    0

let listOfCaves = generateCaves(cavePaths, [])
buildCaveMap cavePaths