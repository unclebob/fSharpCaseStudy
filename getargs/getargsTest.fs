module GetargsTest
open NUnit.Framework
open FsUnit

//temp = 99;
//let config = getargs(argv, "n#,s*,d%")
//let n = "n" |> getArg config

//myCommand -s Bob -d 1.3 -n 42

let makeTokens schema = 
     schema

let getArgs schema args = 
    [],args
    


[<Test>]
let ``no arguments and no schema``() =
     let args = []
     let schema = ""
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs |> should haveLength 0
     unfoundArgs |> should haveLength 0
     
[<Test>]
let ``no schema but with args``() =
     let args = ["-n"; "1"; "-d"; "3.14"]
     let schema = ""
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs |> should haveLength 0 
     unfoundArgs |> should haveLength 4   
     
[<Test>]
let ``break simple schema into tokens``() =    
     let schema = "b"
     schema |> makeTokens |> should equal ["b"]
     

     
[<Test>]
[<Ignore("integration test for later")>]
let ``single int schema with appropriate arg``() =
     let args = ["-b"]
     let schema = "b" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs |> should haveLength 1 
     unfoundArgs |> should haveLength 0    
     
   
     

     

     


     

     