#r "nuget:unquote"
#r "nuget:FParsec"
open System
open System.IO
open Swensen.Unquote
open FParsec

let inputFile = __SOURCE_DIRECTORY__ + "/day2.input"

type Constraint = { min: int; max: int; letter: char }

let getMinMax = pint32 .>> skipChar '-' .>>. pint32 .>> spaces
let getLetter = anyChar .>> skipChar ':' .>> spaces
let getPassword = manyChars anyChar 

let parse = pipe3 getMinMax getLetter getPassword (fun (min, max) letter pwd -> ({min = min; max = max; letter = letter}, pwd)) 

let count x = Seq.filter ((=) x) >> Seq.length

let policy1 c pwd = 
    let n = count c.letter pwd
    n >= c.min && n <= c.max

let policy2 c pwd = 
    if  c.max > String.length(p)
        then false
        else (pwd.[c.min-1] = c.letter) != (pwd.[c.max-1] = c.letter)

let verifyPassword policy s =
    let parseResult = run parse s
    match parseResult with
    | Success (r,_,_) -> r ||> policy
    | Failure (_,_,_) -> false

let verifyPassword1 = verifyPassword policy1
let verifyPassword2 = verifyPassword policy2

verifyPassword1 "3-5 a: aaaaa" =! true
verifyPassword1 "3-5 a: aa" =! false
verifyPassword1 "3-5 a: aaaaaaa" =! false
verifyPassword1 "1-2 a: abab" =! true

verifyPassword2 "1-3 a: abcde" =! true
verifyPassword2 "1-3 b: cdefg" =! false
verifyPassword2 "2-9 c: ccccccccc" =! false

let result1 = 
    File.ReadAllLines(inputFile) |> Seq.map verifyPassword1 |> Seq.filter (fun x -> x) |> Seq.length

result1 =! 418

let result2 = 
    File.ReadAllLines(inputFile) |> Seq.map verifyPassword2 |> Seq.filter (fun x -> x) |> Seq.length

result2 =! 616