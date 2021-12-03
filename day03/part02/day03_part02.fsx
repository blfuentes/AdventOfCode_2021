﻿open System
open System.IO

//let path = "day03_input.txt"
let path = "test_input.txt"
let inputLines = File.ReadLines(__SOURCE_DIRECTORY__ + @"../../" + path) |> Seq.map (fun x -> x.ToCharArray() |> Array.map string) |> Seq.toList

let calculateGammaRate(input: list<string[]>) =
    let maxIdx = input.Head.Length - 1
    let tmp = seq {
        for i in 0 .. maxIdx do
            //printfn "%i" i
            let numberOfOnes = input |> List.map(fun l -> l.[i]) |> List.filter (fun e -> e = "1") |> List.length
            match numberOfOnes >= (input.Length / 2) with
            | true -> yield "1"
            | false -> yield "0"
    }
    tmp |> Seq.toList

let gammarate = calculateGammaRate(inputLines)
let gammarateValue = Convert.ToInt32(gammarate |> List.fold (+) "", 2)
let epsilonrate = gammarate |> List.map(fun x -> if x = "1" then "0" else "1")
let epsilonrateValue = Convert.ToInt32(epsilonrate |> List.fold (+) "", 2)

// part 1
let powerconsumption = gammarateValue * epsilonrateValue
powerconsumption

// part 2
let firstBitOfReport = inputLines |> List.map (fun x -> x.[0])

let rec caculateCriteriaRating(criteria: string[], input: List<string[]>, pos: int) =
    match input.Length with
    | 1 -> input.Head |> Array.toList
    | _ -> caculateCriteriaRating(criteria, (input |> List.filter(fun x -> x.[pos] = criteria.[pos])), pos + 1)

let oxygengeneratorrating = caculateCriteriaRating (gammarate |> List.toArray, inputLines, 0)
let oxygengeneratorratingValue = Convert.ToInt32(oxygengeneratorrating |> List.fold (+) "", 2)
let co2scrubberrating = caculateCriteriaRating (epsilonrate |> List.toArray, inputLines, 0)
let co2scrubberratingValue = Convert.ToInt32(co2scrubberrating |> List.fold (+) "", 2)

let lifesupportrating = oxygengeneratorratingValue * co2scrubberratingValue
printf "%A" lifesupportrating