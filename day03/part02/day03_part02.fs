module day03_part02

open System
open System.IO

let path = "day03_input.txt"
//let path = "test_input.txt"
let inputLines = File.ReadLines(__SOURCE_DIRECTORY__ + @"../../" + path) |> Seq.map (fun x -> x.ToCharArray() |> Array.map string) |> Seq.toList

let calculateRate(input: list<string[]>, criteria: string, noncriteria: string, op) =
    let maxIdx = input.Head.Length - 1
    let tmp = seq {
        for i in 0 .. maxIdx do
            let numberOfCriteria = input |> List.map(fun l -> l.[i]) |> List.filter (fun e -> e = criteria) |> List.length
            let numberOfNonCriteria = input |> List.map(fun l -> l.[i]) |> List.filter (fun e -> e = noncriteria) |> List.length
            match op numberOfCriteria  numberOfNonCriteria with
            | true -> yield criteria
            | false -> yield noncriteria
    }
    tmp |> Seq.toArray

let rec caculateCriteriaRating(criteria: string, noncriteria: string, input: List<string[]>, pos: int, op) =
    match input.Length with
    | 1 -> input.Head |> Array.toList
    | _ -> 
        let newCriteria = calculateRate(input, criteria, noncriteria, op)
        caculateCriteriaRating(criteria, noncriteria, (input |> List.filter(fun x -> x.[pos] = newCriteria.[pos])), pos + 1, op)

let oxygengeneratorrating = caculateCriteriaRating ("1", "0", inputLines, 0, (>=))
let oxygengeneratorratingValue = Convert.ToInt32(oxygengeneratorrating |> List.fold (+) "", 2)
let co2scrubberrating = caculateCriteriaRating ("0", "1", inputLines, 0, (<=))
let co2scrubberratingValue = Convert.ToInt32(co2scrubberrating |> List.fold (+) "", 2)

let execute =
    oxygengeneratorratingValue * co2scrubberratingValue