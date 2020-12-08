#r "nuget:unquote"
open System.IO
open Swensen.Unquote

let inputFile = __SOURCE_DIRECTORY__ + "/day1.input"
let input = 
    File.ReadAllLines(inputFile) |> Array.map int |> Array.toList

let rec searchSumAndMutiply searchSum input s = 
    match input with
    | [] -> -1
    | head::tail -> 
        let foundMatch = searchSum tail (s - head)
        if foundMatch = -1 then searchSumAndMutiply searchSum tail s else foundMatch * head

let searchSumAndMutiply1 = searchSumAndMutiply (fun l s ->  if s = 0 then 1 else -1)
let searchSumAndMutiply2 = searchSumAndMutiply SearchSumAndMutiply1
let searchSumAndMutiply3 = searchSumAndMutiply SearchSumAndMutiply2


searchSumAndMutiply2 input 2020 =! 1016131
searchSumAndMutiply3 input 2020 =! 276432018
