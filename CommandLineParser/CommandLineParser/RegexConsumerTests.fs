module RegexConsumerTests
    // Daniel's late night timeboxed code
    // ONE HOUR
    open System.Text.RegularExpressions
    open FsUnit
    open NUnit.Framework
    //NOTE: After using NuGet to get FsUnit, I had to go back and
    // specifically get the 3.0 version of the NUnit Test Adapter
    // for test discovery to work
    [<Test>]
    let ``testing works``()=
        //1 |> should equal 2
        1 |> should equal 1

    let sampleSchema="fbns*x#rp* a#me*"
    type typesICanMake= Abool|AnInt|AString
    let consumeARegExAndReturnAReducedStringAndAListOFMatchesFound (theRegexToConsume:string) (theStringToConsume:string) =
        let matchesFoundInTheStringToConsume =
            // put in the conditional based on the errors match collection throws
            if (theStringToConsume<>""&&theRegexToConsume<>"")
                then Regex.Matches(theStringToConsume,theRegexToConsume) |> Seq.cast<Match> |> Seq.toList
                else List<Match>.Empty
        let consumeOneMatchFromAString (theMatch:Match) (theString:string) =
            let indexOfTheMatchInTheString=theMatch.Index
            let lengthOfTheMatchingText=theMatch.Length
            // This is the only place I've actually typed a numeric value
            // It could be off-by-one. (It also should be a library function, not a
            // program one)
            let stringWithoutTheMatch=
                theString.Substring(0,indexOfTheMatchInTheString) +
                theString.Substring(indexOfTheMatchInTheString+lengthOfTheMatchingText)
            (stringWithoutTheMatch,theMatch.Value)
        let testReturn=("remainingString",["foundText"])
        matchesFoundInTheStringToConsume |> List.fold(fun acc matchItem->
            let remainingString,matchesFoundSoFar=acc 
            let matchWithModfiedString=
                Regex.Matches(remainingString,theRegexToConsume) 
                |> Seq.cast<Match> |> Seq.toList |> List.head
            let stringWithoutMatch,textWeFound=consumeOneMatchFromAString matchWithModfiedString remainingString
            let newListOfMatches=matchesFoundSoFar|>List.append [textWeFound]
            let newAcc=stringWithoutMatch,newListOfMatches
            newAcc
            ) (theStringToConsume, [])

    //Written afterwards to check boundary conditions
    [<Test>]
    let ``Input String empty``()=
        let stringToParse=""
        let regexToFind="%A*"
        let newString,stringFoundList=consumeARegExAndReturnAReducedStringAndAListOFMatchesFound regexToFind stringToParse
        newString|>should equal stringToParse
        stringFoundList|> should haveLength 0
    [<Test>]
    let ``Regex string empty``()=
        let stringToParse="asdf"
        let regexToFind=""
        let newString,stringFoundList=consumeARegExAndReturnAReducedStringAndAListOFMatchesFound regexToFind stringToParse
        newString|>should equal stringToParse
        stringFoundList|> should haveLength 0
    [<Test>]
    let ``No matches found``()=
        let stringToParse="The quick dog ran over the k dog"
        let regexToFind="%A*"
        let newString,stringFoundList=consumeARegExAndReturnAReducedStringAndAListOFMatchesFound regexToFind stringToParse
        newString|>should equal stringToParse
        stringFoundList|> should haveLength 0
    [<Test>]
    let ``A match found``()=
        let stringToParse="The quick dog ran over the k* dog"
        let regexToFind="[a-z]\*"
        let newString,stringFoundList=consumeARegExAndReturnAReducedStringAndAListOFMatchesFound regexToFind stringToParse
        newString|>should equal "The quick dog ran over the  dog"
        stringFoundList|> should haveLength 1
    //This took extra 20 minutes because of mutating string
    [<Test>]
    let ``Bunch of matches found``()=
        let stringToParse="The quick dog b*k*was a cat"
        let regexToFind="[a-z]\*"
        let newString,stringFoundList=consumeARegExAndReturnAReducedStringAndAListOFMatchesFound regexToFind stringToParse
        newString|>should equal "The quick dog was a cat"
        stringFoundList|> should haveLength 2
