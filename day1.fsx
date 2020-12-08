open System
open System.IO

let inputFile = __SOURCE_DIRECTORY__ + "/day1.input"
let input = 
    File.ReadAllLines(inputFile) |> Array.map int |> Array.toList

let FindMatch1 input s = 
    let found = 
        input 
        |> List.map (fun x -> if x = s then x else -1)
        |> List.filter (fun x -> x >= 0)
    if Seq.isEmpty found then -1 else Seq.head found


let rec FindMatch2 input s = 
    match input with
    | [] -> -1
    | head::tail -> 
        let foundMatch = FindMatch1 tail (s - head)
        if foundMatch = -1 then FindMatch2 tail s else foundMatch * head

let rec FindMatch3 input s = 
    match input with
    | [] -> -1
    | head::tail -> 
        let foundMatch = FindMatch2 tail (s - head)
        if foundMatch = -1 then FindMatch3 tail s else foundMatch * head


let a = [5; 1; 34; 55; 20; 6; 1000; 2000]
FindMatch2 a 2020
FindMatch2 input 2020