module Utilities

open System
open System.IO
open System.Text.RegularExpressions

let GetLinesFromFile(path: string) =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + @"../../" + path)

let GetLinesFromFileFSI2(path: string) =
    File.ReadAllLines(path)

let GetLinesFromFileFSI(path: string) =
    File.ReadLines(path)

let rec combination (num, list: 'a list) : 'a list list = 
    match num, list with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combination ((k-1), xs)) @ (combination (k, xs))

let possibleCombinations (combSize: int) (mainList: uint64 list) =
    seq {
        for init in mainList do
            let index = mainList |> List.findIndex(fun e -> e = init)
            if mainList.Length - combSize > index then
                yield mainList |> List.skip(index) |> List.take(combSize)
    } |> List.ofSeq

let getLinesGroupBySeparator (inputLines: string list) (separator: string) =
    let complete = 
        seq {
            for line in inputLines do
                yield! line.Split(' ')
        } |> List.ofSeq
    let folder (a) (cur, acc) = 
        match a with
        | _ when a <> separator -> a::cur, acc
        | _ -> [], cur::acc
    
    let result = List.foldBack folder (complete) ([List.last complete], []) 
    (fst result)::(snd result)


let getLinesGroupBySeparator2 (inputLines: string list) (separator: string) =
    let complete = 
        seq {
            for line in inputLines do
                yield! line.Split(' ')
        } |> List.ofSeq
    let folder (a) (cur, acc) = 
        match a with
        | _ when a <> separator -> a::cur, acc
        | _ -> [], cur::acc
        
    let result = List.foldBack folder (complete) ([], [])
    (fst result)::(snd result)

let folder (a) (cur, acc) = 
    match a with
    | _ when a <> 0 -> a::cur, acc
    | _ -> [], cur::acc

let split lst =
    let result = List.foldBack folder (lst) ([], [])
    (fst result)::(snd result)

let updateElement index element list = 
  list |> List.mapi (fun i v -> if i = index then element else v)

// XOR OPERATOR
let (^@) (a: bool) (b:bool) : bool =
    a <> b

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let getCollisionsBasic (currentForest: list<int[]>) initX initY right down maxwidth maxheight =
    let positions = [initY..down..maxheight]
    seq {
        for pos in initY..down..maxheight do
            let currentPos = positions |> List.findIndex (fun x -> x = pos)
            let point = [|((initX + right) * (currentPos + 1)) % maxwidth; pos + down|]
            match currentForest |> List.exists (fun t -> t.[0] = point.[0] && t.[1] = point.[1]) with 
            | true -> yield point
            | _ -> ()
    } |> Seq.length

let byrValid (elem:string) =
    elem.Length = 4 && (elem |> int) >= 1920 && (elem |> int) <= 2002

let iyrValid (elem:string) =
    elem.Length = 4 && (elem |> int) >= 2010 && (elem |> int) <= 2020

let eyrValid (elem:string)=
    elem.Length = 4 && (elem |> int) >= 2020 && (elem |> int) <= 2030

let hgtValid (elem:string)=
    let parts =
        match elem with
        | Regex @"(?<height>\d+)(?<unittype>\w+)" [m; M] -> Some { height= m |> int; unittype = M }
        | _ -> None
    match parts with
    | Some { HeightType.height = height; HeightType.unittype = unittype; } when unittype = "cm" -> height >= 150 && height <= 193
    | Some { HeightType.height = height; HeightType.unittype = unittype; } when unittype = "in" -> height >= 59 && height <= 76
    | _ -> false

let hclValid (elem:string)=
    match elem with
    | Regex @"#[0-9a-f]{6}" result -> true
    | _ -> false

let concatStringList (list:string list) =
    seq {
        for l in list do
            yield! l.ToCharArray()
    } |> List.ofSeq

let concatStringListSeparated (list:string list) =
    seq {
        for l in list do
            yield l.ToCharArray()
    } |> List.ofSeq

let commonElements (input: char array list) =
    let inputAsList = input |> List.map (List.ofArray)
    let inputAsSet = List.map Set.ofList inputAsList
    let elements =  Seq.reduce Set.intersect inputAsSet
    elements

let commonElements2 (input: char array list) =
    input |> List.map (List.ofArray) |> Seq.map Set.ofList |> Set.intersectMany
