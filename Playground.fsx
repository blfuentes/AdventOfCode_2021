List.iter (printf "%d") [0 .. 9]
List.iter 
    (printf "%d") 
        [0 .. 9]

let calculateRate(input: list<string[]>, criteria: string, noncriteria: string, op: (int -> int -> bool)) =
    let rate = [0 .. input.Head.Length - 1] 
            |> List.map(fun idx -> input 
                                |> List.filter (fun e -> e.[idx] = criteria) 
                                |> List.length)

    rate |> List.map(fun r -> if op r (input.Length - r) then criteria else noncriteria) |> List.toArray

let fakeinput = [[|"0";"0";"0";"1"|];[|"0";"0";"1";"0"|];[|"0";"0";"1";"1"|];[|"0";"1";"0";"0"|];]
let rate = calculateRate(fakeinput, "1", "0", (>=))
rate