module GetargsTest
open NUnit.Framework
open FsUnit

//temp = 99;
//let config = getargs(argv, "n#,s*,d%")
//let n = "n" |> getArg config

//myCommand -s Bob -d 1.3 -n 42

let breakString (s : string) =
     s.ToCharArray() |> Array.map(fun (c:char)->c.ToString())

let makeTokens schema =
     [schema]
    
let getArgs (schema:string) (args : string list) = 
     let unmarkedArgs = args |> List.map(fun s -> s.Replace("-", ""))
     unmarkedArgs |> List.partition(fun arg -> schema.IndexOf(arg) <> -1)
    
let getBoolean (foundArgs : string list) flag =
     foundArgs |> List.exists(fun x -> x = flag)
    
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
let ``break string into sequence of single char strings``() =
     "abc" |> breakString |> should equal ["a";"b";"c"]  
    
[<Test>]
let ``single boolean schema without appropriate arg``() =
     let args = ["-f"]
     let schema = "b" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs |> should haveLength 0 
     unfoundArgs |> should haveLength 1 
     "b" |> getBoolean foundArgs |> should equal false   
    
[<Test>]
let ``single boolean schema with appropriate arg``() =
     let args = ["-f"]
     let schema = "f" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs |> should haveLength 1 
     unfoundArgs |> should haveLength 0 
     "f" |> getBoolean foundArgs |> should equal true         
     
[<Test>]
let ``single boolean schema with one appropriate arg and one inappropriate arg``() =
     let args = ["-f";"-b"]
     let schema = "f" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs |> should haveLength 1 
     unfoundArgs |> should equal ["b"] 
     "f" |> getBoolean foundArgs |> should equal true   
     

     


     

     