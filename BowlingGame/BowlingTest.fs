module BowlingTest
open NUnit.Framework
open FsUnit

type List<'a> with
    static member Prepend prefix list  =
        prefix |> List.append list   

let scoreFrames rolls =
    let rec loop rolls scores =
        match rolls with
            | [] -> scores
            | 10 :: r1 :: r2 :: rest ->
                loop (List.skip 1 rolls) 
                    ([10+r1+r2] |> List.append scores)
            | r1::r2::r3::rest when r1+r2=10 -> 
                loop (List.skip 2 rolls) 
                    ([r1+r2+r3] |> List.append scores)
            | r1::r2::rest -> 
                loop (List.skip 2 rolls) 
                    ([r1+r2] |> List.append scores)
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






