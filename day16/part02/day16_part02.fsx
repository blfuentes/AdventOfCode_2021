open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Globalization  

#load @"../../Modules/Utilities.fs"

let hexToBin(hex: string) =
    match hex with
    | "0" -> "0000"
    | "1" -> "0001"
    | "2" -> "0010"
    | "3" -> "0011"
    | "4" -> "0100"
    | "5" -> "0101"
    | "6" -> "0110"
    | "7" -> "0111"
    | "8" -> "1000"
    | "9" -> "1001"
    | "A" -> "1010"
    | "B" -> "1011"
    | "C" -> "1100"
    | "D" -> "1101"
    | "E" -> "1110"
    | "F" -> "1111"
    | _ -> "0000"
let binToDec(bin: string) =
    Convert.ToInt64(bin, 2)

let typeToOp(typeid: int64) =
    match int32(typeid) with
    | 4 -> "lit"
    | 0 -> "sum"
    | 1 -> "prod"
    | 2 -> "min"
    | 3 -> "max"
    | 5 -> "greater"
    | 6 -> "lesser"
    | 7 -> "equal"
    | _ -> ""

let numberOfSubpackets(typeid: int64) =
    match int32(typeid) with
    | 4 -> 1
    | 0 -> -1
    | 1 -> -1
    | 2 -> -1
    | 3 -> -1
    | 5 -> 2
    | 6 -> 2
    | 7 -> 2
    | _ -> 0

let operations = ["sum"; "prod"; "min"; "max"; "greater"; "lesser"; "equal"]

type Element =
    {
        ParentPackageId: int
        PackageId: int
        Version: int64
        Op: string
    }

let rec parseMessage(mymessage: string, parentpackage:int, currentStack: Stack<Element>) =
    match mymessage.Contains("1") && mymessage.Length >= 7 with
    | false -> mymessage
    | true -> 
        let version = binToDec(mymessage.Substring(0, 3))
        let typeid = binToDec(mymessage.Substring(3, 3))
        let op = typeToOp(typeid)
        let numOfSubpackets = numberOfSubpackets(typeid)
        match int32(typeid) with
        | 4 -> 
            let newpackageid = parentpackage + 1
            let number = mymessage.Substring(6)
            let parts = number.ToCharArray() |> Array.chunkBySize(5) |> Array.map(fun chunk -> String.Join("", chunk))
            let splitIdx = parts |> Array.findIndex(fun p -> p.Substring(0, 1) = "0")
            let usedParts = parts |> Array.take(splitIdx + 1)
            let usedPartsLength = usedParts |> Array.sumBy(fun l -> l.Length)
            let remainingPart = mymessage.Substring(6 + usedPartsLength)
            let finalnumber = String.Join("", usedParts |> Array.map(fun p -> p.Substring(1, 4)))
            let newElement = {
                ParentPackageId= parentpackage
                PackageId= newpackageid
                Version= version
                Op= binToDec(finalnumber).ToString()
            }
            currentStack.Push(newElement)
            parseMessage(remainingPart, newElement.PackageId, currentStack) 
        | _ ->
            let newpackageid = parentpackage + 1
            let newElement = {
                ParentPackageId= parentpackage
                PackageId= newpackageid
                Version= version
                Op= op
            }
            currentStack.Push(newElement)
            let lenghtypeid = mymessage.Substring(6, 1)
            match lenghtypeid with
            | "0" ->
                let adjustedMymessage = 
                    if(22 - mymessage.Length > 0) then
                        mymessage + (new String('0', 22 - mymessage.Length))
                    else
                        mymessage
                let totallength = binToDec(adjustedMymessage.Substring(7, 15))
                let subpacket = adjustedMymessage.Substring(22)
                parseMessage(subpacket, newpackageid, currentStack) 

                //parseMessage(subpacket.Substring(0, int(totallength)), newpackageid, currentStack) 
            | "1" ->
                let adjustedMymessage = 
                    if (18 - mymessage.Length > 0) then
                        mymessage + (new String('0', 18 - mymessage.Length))
                    else
                        mymessage
                let numberofpackets = int32(binToDec(adjustedMymessage.Substring(7, 11)))
                let subpackets = adjustedMymessage.Substring(18)
                parseMessage(subpackets, newpackageid, currentStack) 

let performOp(myOp: string, values: string list) =
    match myOp with
    | "sum" -> values |> List.map(fun v -> Int64.Parse(v)) |> List.sum
    | "prod" -> values |> List.map(fun v -> Int64.Parse(v)) |> List.fold (*) 1
    | "min" -> values |> List.map(fun v -> Int64.Parse(v)) |> List.sort |> List.head
    | "max" -> values |> List.map(fun v -> Int64.Parse(v)) |> List.sortDescending |> List.head
    | "lesser" -> 
        let tmpValues = [|(values |> List.map(fun v -> Int64.Parse(v))) |> List.head; (values |> List.map(fun v -> Int64.Parse(v))) |> List.tail |> List.head|]
        if tmpValues.[1] < tmpValues.[0] then 1 else 0
    | "greater" -> 
        let tmpValues = [|(values |> List.map(fun v -> Int64.Parse(v))) |> List.head; (values |> List.map(fun v -> Int64.Parse(v))) |> List.tail |> List.head|]
        if tmpValues.[1] > tmpValues.[0] then 1 else 0
    | "equal" -> if (values |> List.map(fun v -> Int64.Parse(v))) |> List.pairwise |> List.forall (fun (a,b) -> a = b) then 1 else 0
    | _ -> int64(0)

let rec atomizePart(currentlist: string list, myops: string list, parts: (string * string list) list) =
    match currentlist |> List.filter(fun e -> myops |> List.contains(e)) |> List.length with
    | 1 -> 
        let op = (currentlist |> List.rev).Head
        let values = (currentlist |> List.takeWhile(fun e -> not(myops |> List.contains(e))))
        parts @ [(op, values)]
    | _ -> 
        let idx = currentlist |> List.findIndex(fun e -> myops |> List.contains(e))
        let subs = currentlist |> List.splitAt(idx)
        let newPart = ((snd subs).Head, (fst subs))
        atomizePart((snd subs).Tail, myops, [newPart] @ parts)

let rec processResult(currentlist: string list, myops: string list) =
    let requiresSub = currentlist |> List.pairwise |> List.exists(fun (a, b) -> (myops |> List.contains(a)) && (myops |> List.contains(b)))
    
    match requiresSub with
    | true -> 
        let splitIdx = currentlist |> List.pairwise |> 
                        List.findIndex(fun (a, b) -> (myops |> List.contains(a)) && (myops |> List.contains(b)))
        let parts = currentlist |> List.splitAt(splitIdx)
        let values = fst parts
        let op::rest = snd parts
        let operationsparts = values @ [op]
        let result = atomizePart(operationsparts, myops, []) |> List.map(fun o -> performOp(fst o, snd o).ToString())
        match rest with
        | [] -> 
            let op = (rest |> List.rev).Head
            let values = (rest |> List.takeWhile(fun e -> not(myops |> List.contains(e))))
            performOp(op, rest)
        | head::tail -> processResult(result @ rest, myops)
    | false -> 
        let op = (currentlist |> List.rev).Head
        let values = (currentlist |> List.takeWhile(fun e -> not(myops |> List.contains(e))))
        performOp(op, values)

let path = "day16_input.txt"   // (871)
//let path = "test_input_00.txt" // (6)
//let path = "test_input_01.txt" // (9)
//let path = "test_input_02.txt" // (14)
//let path = "test_input_03.txt" // (16)
//let path = "test_input_04.txt" // (12)
//let path = "test_input_05.txt" // (23)
//let path = "test_input_06.txt" // (31)          
 
let message = 
    File.ReadLines(__SOURCE_DIRECTORY__ + @"../../" + path) |> 
        Seq.map(fun l -> String.Join("", l.ToCharArray() |> Array.map(fun c -> hexToBin((string)c))))

let tmpmessage = message |> Seq.exactlyOne 

let opStack = new Stack<Element>()
parseMessage(tmpmessage, 0, opStack)


let xx =
    seq {
        while opStack.Count > 0 do
            let value = opStack.Pop()
            if value.Op <> "lit" then yield value
    } |> Seq.toList// |> List.rev
let result = xx |> List.sumBy(fun x -> x.Version)
let values = xx |> List.map(fun v -> v.Op)
printfn "%s", String.Join(",", values)
processResult(values, operations)

let samples = [|"C200B40A82";                   // "2"; "1"; "sum" -> 3
                "04005AC33890";                 // "9"; "6"; "prod" -> 54
                "880086C3E88112";               // "9"; "8"; "7"; "min" -> 7
                "CE00C43D881120";               // "9"; "8"; "7"; "max" -> 9
                "D8005AC2A8F0";                 // "15"; "5"; "lesser" -> 1
                "F600BC2D8F";                   // "15"; "5"; "greater" -> 0
                "9C005AC2F8F0";                 // "15"; "5"; "equal" -> 0
                "9C0141080250320F1802104A08"|]  // "2"; "2"; "prod"; "3"; "1"; "sum"; "equal" -> 1
let tmpmessage2 = String.Join("", samples.[6].ToCharArray() |> Array.map(fun c -> hexToBin((string)c)))

let opStack2 = new Stack<Element>()
parseMessage(tmpmessage2, 0, opStack2)

let xxx =
    seq {
        while opStack2.Count > 0 do
            let value = opStack2.Pop()
            if value.Op <> "lit" then yield value
    } |> Seq.toList// |> List.rev
let result2 = xxx |> List.sumBy(fun x -> x.Version)
let values2 = xxx |> List.map(fun v -> v.Op)
printfn "%s", String.Join(",", values2)


processResult(values2, operations)
//processResult(values, operations)