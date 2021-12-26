open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Globalization  

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

let rec parseMessage(mymessage: string, versions: int64 list, currentStack: Stack<string>) =
    let version = binToDec(mymessage.Substring(0, 3))
    let typeid = binToDec(mymessage.Substring(3, 3))
    let op = typeToOp(typeid)
    currentStack.Push(op)
    match int32(typeid) with
    | 4 -> 
        let number = mymessage.Substring(6)
        let parts = number.ToCharArray() |> Array.chunkBySize(5) |> Array.map(fun chunk -> String.Join("", chunk))
        let splitIdx = parts |> Array.findIndex(fun p -> p.Substring(0, 1) = "0")
        let usedParts = parts |> Array.take(splitIdx + 1)
        let usedPartsLength = usedParts |> Array.sumBy(fun l -> l.Length)
        let remainingPart = mymessage.Substring(6 + usedPartsLength)
        let finalnumber = String.Join("", usedParts |> Array.map(fun p -> p.Substring(1, 4)))
        currentStack.Push(binToDec(finalnumber).ToString())
        versions @ [version] @ 
                            if (remainingPart.Contains("1") && remainingPart.Length >= 7) then 
                                parseMessage(remainingPart, [], currentStack) 
                            else 
                                []
    | _ ->
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
            [version] @ 
                    if (subpacket.Contains("1") && subpacket.Length >= 7) then 
                        parseMessage(subpacket, [], currentStack) 
                    else 
                        []
        | "1" ->
            let adjustedMymessage = 
                if (18 - mymessage.Length > 0) then
                    mymessage + (new String('0', 18 - mymessage.Length))
                else
                    mymessage
            let numberofpackets = binToDec(adjustedMymessage.Substring(7, 11))
            currentStack.Push("(" + numberofpackets.ToString() + ")")
            let subpackets = adjustedMymessage.Substring(18)
            [version] @ 
                    if (subpackets.Contains("1") && subpackets.Length >= 7) then 
                        parseMessage(subpackets, [], currentStack) 
                    else 
                        []

let path = "day16_input.txt"
//let path = "test_input_all.txt"
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

let opStack = new Stack<string>()
parseMessage(tmpmessage, [], opStack) |> List.sum

let values =
    seq {
        while opStack.Count > 0 do
            let value = opStack.Pop()
            if value <> "lit" then yield value
    } |> Seq.toList |> List.rev

printfn "%s", String.Join(",", values)

let samples = [|"C200B40A82";                   // "2"; "1"; "sum" -> 3
                "04005AC33890";                 // "9"; "6"; "prod" -> 54
                "880086C3E88112";               // "9"; "8"; "7"; "min" -> 7
                "CE00C43D881120";               // "9"; "8"; "7"; "max" -> 9
                "D8005AC2A8F0";                 // "15"; "5"; "lesser" -> 1
                "F600BC2D8F";                   // "15"; "5"; "greater" -> 0
                "9C005AC2F8F0";                 // "15"; "5"; "equal" -> 0
                "9C0141080250320F1802104A08"|]  // "2"; "2"; "prod"; "3"; "1"; "sum"; "equal" -> 1
let tmpmessage2 = String.Join("", samples.[7].ToCharArray() |> Array.map(fun c -> hexToBin((string)c)))

let opStack2 = new Stack<string>()
parseMessage(tmpmessage2, [], opStack2)

let values2 =
    seq {
        while opStack2.Count > 0 do
            let value = opStack2.Pop()
            //yield value
            if value <> "lit" then yield value
    } |> Seq.toList |> List.rev

printfn "%s", String.Join(",", values2)


let operations = ["sum"; "prod"; "min"; "max"; "greater"; "lesser"; "equal"]

//let performOp(myOp: string, values: string list) =
//    match myOp with
//    | "sum" -> values |> List.map(fun v -> Int32.Parse(v)) |> List.sum
//    | "prod" -> values |> List.map(fun v -> Int32.Parse(v)) |> List.fold (*) 1
//    | "min" -> values |> List.map(fun v -> Int32.Parse(v)) |> List.sort |> List.head
//    | "max" -> values |> List.map(fun v -> Int32.Parse(v)) |> List.sortDescending |> List.head
//    | "lesser" -> 
//        let tmpValues = [|(values |> List.map(fun v -> Int32.Parse(v))) |> List.head; (values |> List.map(fun v -> Int32.Parse(v))) |> List.tail |> List.head|]
//        if tmpValues.[1] < tmpValues.[0] then 1 else 0
//    | "greater" -> 
//        let tmpValues = [|(values |> List.map(fun v -> Int32.Parse(v))) |> List.head; (values |> List.map(fun v -> Int32.Parse(v))) |> List.tail |> List.head|]
//        if tmpValues.[1] > tmpValues.[0] then 1 else 0
//    | "equal" -> if (values |> List.map(fun v -> Int32.Parse(v))) |> List.pairwise |> List.forall (fun (a,b) -> a = b) then 1 else 0
//    | _ -> 0  


let rec performOp(myOp: string, values: string list) =
    match myOp with
    | "lit" -> values.Head |> int64
    | "sum" -> values |> List.map(fun v -> Int64.Parse(v)) |> List.sum
    | "prod" -> values |> List.map(fun v -> Int64.Parse(v)) |> List.fold (*) 1
    | "min" -> values |> List.map(fun v -> Int64.Parse(v)) |> List.sort |> List.head
    | "max" -> values |> List.map(fun v -> Int64.Parse(v)) |> List.sortDescending |> List.head
    | "lesser" -> 
        let tmpValues = [|(values |> List.map(fun v -> Int64.Parse(v))) |> List.head; (values |> List.map(fun v -> Int64.Parse(v))) |> List.tail |> List.head|]
        if tmpValues.[0] < tmpValues.[1] then int64(1) else int64(0)
    | "greater" -> 
        let tmpValues = [|(values |> List.map(fun v -> Int64.Parse(v))) |> List.head; (values |> List.map(fun v -> Int64.Parse(v))) |> List.tail |> List.head|]
        if tmpValues.[0] > tmpValues.[1] then int64(1) else int64(0)
    | "equal" -> if (values |> List.map(fun v -> Int64.Parse(v))) |> List.pairwise |> List.forall (fun (a,b) -> a = b) then int64(1) else int64(0)
    | _ -> int64(0)


let rec processResult(currentlist: string list, myops: string list) =
    let numberOfOperations = (currentlist |> List.filter(fun e -> myops |> List.contains(e))) |> List.length
    match numberOfOperations with
    | 1 -> performOp(currentlist.Head, currentlist.Tail).ToString()
    | _ -> 
        performOp(currentlist.Head, [processResult(currentlist.Tail, myops)]).ToString()

//let rec processResult(currentlist: string list) =
//    let splitIdx = currentlist |> List.findIndex(fun e -> operations |> List.contains(e))
//    let parts = currentlist |> List.splitAt(splitIdx)
//    let values = fst parts
//    let op::rest = snd parts
//    let result = performOp(op, values)
//    match rest with
//    | [] -> result
//    | head::tail -> processResult([result.ToString()] @ rest)

processResult(values2, operations)
processResult(values, operations)