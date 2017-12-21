module BowlingTest
open NUnit.Framework
open FsUnit
// Note: you need the Fsharp.Core dll of version 4.1 or better

type List<'a> with
    static member Prepend prefix list  =
        prefix |> List.append list   

//This is the implementation from the video. Note the match is not complete (BAD!)
let scoreFrames rolls =
    let rec loop rolls scores =
        match rolls with
            | [] -> scores
            | 10 :: r1 :: r2 :: rest ->
                loop ( List.skip 1 rolls) 
                    ([10+r1+r2] |> List.append scores)
            | r1::r2::r3::rest when r1+r2=10 -> 
                loop (List.skip 2 rolls) 
                    ([r1+r2+r3] |> List.append scores)
            | r1::r2::rest -> 
                loop (List.skip 2 rolls) 
                    ([r1+r2] |> List.append scores)
    loop rolls []
    
// Let's try it again
let scoreFrames2 rolls =
    let rec loop rolls scores =
        match rolls with
            | [] -> scores
            | 10 :: r1 :: r2 :: rest ->
                loop ( List.skip 1 rolls) 
                    ([10+r1+r2] |> List.append scores)
            | r1::r2::r3::rest when r1+r2=10 -> 
                loop (List.skip 2 rolls) 
                    ([r1+r2+r3] |> List.append scores)
            | r1::r2::rest -> 
                loop (List.skip 2 rolls) 
                    ([r1+r2] |> List.append scores)
              // This is the missing path
              // Note the lack of squiggly above
              // This should never be called, but it makes
              // the code correct. Never leave open paths
            | r1::r2->raise (new System.ArgumentOutOfRangeException("rolls", "rolls should never have only two items and not match one of our existing cases"))
    loop rolls []

let score rolls =
    let frameScores = rolls |> scoreFrames 
    (List.take 10 frameScores) |> List.sum
    
let addZeroes n rolls =
    rolls |> List.Prepend (List.replicate n 0)

[<Test>]
let ``test a gutter Game``() = 
    List.replicate 20 0 |> score |> should equal 0
    
[<Test>]
let ``test game with one pin``() = 
    [1] |> addZeroes 19 
        |> score |> should equal 1
    
[<Test>]
let ``test game with one spare``() =
    [5;5;3] |> addZeroes 17
        |> score |> should equal 16
        
[<Test>]
let ``test game with one strike``() =
    [10;3;4] |> addZeroes 16
        |> score |> should equal 24
        
[<Test>]
let ``perfect game``() =
    List.replicate 12 10
        |> score |> should equal 300






