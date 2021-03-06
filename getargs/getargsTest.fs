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
          | _ as schemaError -> raise (new System.Exception("Schema error " + string schemaError))

     loop schemaChars Map.empty
     
let makeFlagsAndValues (args : string []) =
     args |> Array.map (fun arg -> 
                       if arg.[0] = '-' 
                            then (arg.Substring(1),true) 
                            else (arg, false))
     |> Array.toList
                 
let getArgs schema args = 
     let expectedArgs = schema |> getExpectedArgs 
     let flagsAndValues = args |> makeFlagsAndValues
     let rec loop flagsAndValues (foundArgs : Map<string, string*string>) (unfoundArgs : string list) =
          match flagsAndValues with
          | [] -> foundArgs,unfoundArgs
          | (flag,true)::rest when not (expectedArgs.ContainsKey flag) ->
               loop rest foundArgs (flag :: unfoundArgs)
          | (flag,true)::rest when expectedArgs.[flag] = "bool" ->
               let newFoundArgs = foundArgs.Add(flag,("bool","true"))
               loop rest newFoundArgs unfoundArgs
          | (flag,true)::(value,false)::rest 
               when (expectedArgs.ContainsKey flag) && expectedArgs.[flag] = "int" ->
                    let newFoundArgs = foundArgs.Add((string flag.[0]),("int",value)) 
                    loop rest newFoundArgs unfoundArgs
          | (value,false)::rest -> loop rest foundArgs (value :: unfoundArgs)
 
          | _ -> raise (new System.Exception("Argument Syntax Error"))
          
     loop flagsAndValues Map.empty []         
     
     
     //unmarkedArgs |> List.partition(fun arg -> schema.IndexOf(arg) <> -1)
     
// {"b" ("bool" "value")}
    
let getBoolean (foundArgs : Map<_,_>) flag =
     if (foundArgs.ContainsKey flag) && (fst foundArgs.[flag] = "bool") 
          then true
          else false
     
let getInt (foundArgs : Map<_,_>) flag = 
     if (foundArgs.ContainsKey flag) && (fst foundArgs.[flag] = "int")
          then System.Int32.Parse (snd foundArgs.[flag])
          else 0
    
[<Test>]
let ``no arguments and no schema``() =
     let args = [||]
     let schema = ""
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs.Count |> should equal 0
     unfoundArgs |> should haveLength 0
     
[<Test>]
let ``no schema but with args``() =
     let args = [|"-n"; "1"; "-d"; "3.14"|]
     let schema = ""
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs.Count |> should equal 0
     unfoundArgs |> should haveLength 4    
    
[<Test>]
let ``single boolean schema without appropriate arg``() =
     let args = [|"-f"|]
     let schema = "b" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs.Count |> should equal 0 
     unfoundArgs |> should haveLength 1 
     "b" |> getBoolean foundArgs |> should equal false   
    
[<Test>]
let ``single boolean schema with appropriate arg``() =
     let args = [|"-f"|]
     let schema = "f" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs.Count |> should equal 1  
     unfoundArgs |> should haveLength 0 
     "f" |> getBoolean foundArgs |> should equal true         
     
[<Test>]
let ``single boolean schema with one appropriate arg and one inappropriate arg``() =
     let args = [|"-f";"-b"|]
     let schema = "f" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs.Count |> should equal 1  
     unfoundArgs |> should equal ["b"] 
     "f" |> getBoolean foundArgs |> should equal true   

[<Test>]
let ``single int schema with inappropriate arg``() =   
     let args = [|"-b"|]
     let schema = "n#" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs.Count |> should equal 0  
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
     let args = [|"-n";"42"|]
     let schema = "n#" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     foundArgs.Count |> should equal 1  
     unfoundArgs |> should haveLength 0
     "n" |> getInt foundArgs |> should equal 42   
     
[<Test>]
let ``asking for boolean arg not in schema``() =
     let args = [|"-f"|]
     let schema = "b" 
     let foundArgs, unfoundArgs = args |> getArgs schema
     "f" |> getBoolean foundArgs

   
     
     
     
     


// next test call getInt on missing arg.    
// spaces in the schema.


     

     