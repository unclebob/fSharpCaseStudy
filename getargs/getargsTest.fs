module GetargsTest
open NUnit.Framework
open FsUnit

//temp = 99;
//let config = getargs(argv, "n#,s*,d%")
//let n = "n" |> getArg config

//myCommand -s Bob -d 1.3 -n 42

let getArgs schema args = 
    []


[<Test>]
let ``no arguments and no schema``() =
     let args = []
     let schema = ""
     let foundArgs, unfoundArgs = args |> getArgs schema
     config |> foundArgs |> should haveLength 0
     
[<Test>]
let ``no schema but with args``() =
     let args = ["-n"; "1"; "-d"; "3.14"]
     let schema = ""
     let config = args |> getArgs schema
     let foundArgs, unfoundArgs = config |> 
     config |> foundArgs |> should haveLength 0 
     config |> unfoundArgs |> should haveLength 4   
     

     


     

     