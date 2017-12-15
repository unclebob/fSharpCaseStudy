module GetargsTest
open NUnit.Framework
open FsUnit

open System.Collections.Generic
open System

//temp = 99;
//let config = getargs(argv, "n#,s*,d%")
//let n = "n" |> getArg config

//myCommand -s Bob -d 1.3 -n 42

// "fbn#s*" -> {f=bool;b=bool;n=int;s=string}




let getExpectedArgs (schema:string) : Map<string,string>  =
     let schemaChars = schema.ToCharArray() |> Array.toList
     let rec loop remainingSchemaChars (argsMap:Map<string,string>) =
          match remainingSchemaChars with
          | [] -> argsMap
          | c::'#'::rest when Char.IsLetter c -> loop rest (argsMap.Add(string c, "int"))
          | c::rest when Char.IsLetter c -> loop rest (argsMap.Add(string c,"bool"))
          | _ -> raise (new System.Exception("blah"))

     loop schemaChars Map.empty
                 
let getArgs (schema:string) (args : string list) = 
     let expectedArgs = schema |> getExpectedArgs 
     let flagsAndValues = args |> List.Map (fun arg -> if arg.[0] = '-' then (arg.substring(1),true) else (arg, false))
     
     
     unmarkedArgs |> List.partition(fun arg -> schema.IndexOf(arg) <> -1)
     
// {"b" ("bool" "value")}
    
let getBoolean (foundArgs : string list) flag =
     foundArgs |> List.exists(fun x -> x = flag)
     
let getInt foundArgs flag = 
     99
    
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

[<Test>]
let ``single int schema with inappropriate arg``() =   
     let args = ["-b"]
     let schema = "n#" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs |> should haveLength 0 
     unfoundArgs |> should equal ["b"] 
     
[<Test>]
let ``expected args for empty schema is empty``() =    
     let expectedArgs = getExpectedArgs ""
     expectedArgs |> Seq.cast |> Seq.length |> should equal 0
     
[<Test>]
let ``expected args for schema with one boolean``() =
     let expectedArgs = getExpectedArgs "b"
     expectedArgs.["b"] |> should equal "bool"
        
[<Test>]
let ``single int schema with one appropriate arg``() =
     let args = ["-n";"42"]
     let schema = "n#" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs |> should haveLength 1 
     unfoundArgs |> should haveLength 0
     "n" |> getInt foundArgs |> should equal 42   

// next test call getInt on missing arg.    
// spaces in the schema.


     

     