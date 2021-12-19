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

let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    returnValue 

// Returns a sublists list of size num: [1;2;3;4;5;6] -> (3) [[1;2;3];[2;3;4];[3;4;5];[4;5;6]]
let rec getSubListBySize (num, list: 'a list) : 'a list list= 
    match num <= list.Length with
    | true -> [(list |> List.take num)] @ (getSubListBySize(num, (list.Tail)))
    | false -> []

let rec combination (num, list: 'a list) : 'a list list = 
    match num, list with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combination ((k-1), xs)) @ (combination (k, xs))

let printArray (board: int[][]) =
    let maxCol = board.[0].Length - 1
    let maxRow = board.Length - 1
    for r in [0 .. maxRow] do
        for c in [0 .. maxCol] do
            printf "%3i" board.[r].[c]
        printfn "%s" System.Environment.NewLine

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

let splitStringBySeparator (content: string) (separator: string) =
    let subcontent = content.Split([|separator|], StringSplitOptions.None)
    subcontent

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

///////////////////////////////////////
// DAY 8 Permutations
let distrib e L =
    let rec aux pre post = 
        seq {
            match post with
            | [] -> yield (L @ [e])
            | h::t -> yield (List.rev pre @ [e] @ post)
                      yield! aux (h::pre) t 
        }
    aux [] L

let rec perms = function 
    | [] -> Seq.singleton []
    | h::t -> Seq.collect (distrib h) (perms t)
///////////////////////////////////////

let folder (a) (cur, acc) = 
    match a with
    | _ when a <> 0 -> a::cur, acc
    | _ -> [], cur::acc

let splitstring separator (s:string) =
    let values = ResizeArray<_>()
    let rec gather start i =
        let add () = s.Substring(start,i-start) |> values.Add
        if i = s.Length then add()
        elif s.[i] = '"' then inQuotes start (i+1) 
        elif s.[i] = separator then add(); gather (i+1) (i+1) 
        else gather start (i+1)
    and inQuotes start i =
        if s.[i] = '"' then gather start (i+1)
        else inQuotes start (i+1)
    gather 0 0
    values.ToArray()

let split lst =
    let result = List.foldBack folder (lst) ([], [])
    (fst result)::(snd result)

let updateElement index element list = 
  list |> List.mapi (fun i v -> if i = index then element else v)

let toJagged<'a> (arr: 'a[,]) : 'a[][] =
    [|for x in 0..Array2D.length1 arr - 1 do
        yield [| for y in 0 ..Array2D.length2 arr - 1 -> arr.[x, y] |]|]

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
