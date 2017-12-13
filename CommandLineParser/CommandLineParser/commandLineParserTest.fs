module CommandLineParserTest

//temp = 99;
//let config = getargs(argv, "n#,s*,d%")
//let n = "n" |> getArg config

//myCommand -s Bob -d 1.3 -n 42

type CommandLineConfigElement<'a> =
    {
        Flag:string
        Value:'a
    }
    
let intConfigList = [{Flag="n";Value=42};{Flag="m";Value=99}]
 


