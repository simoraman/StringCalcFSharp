module stringcalc
open NUnit.Framework
open FsUnit
open System.Text.RegularExpressions

let rec sumList(list, acc) =
    match list with
    | [] -> acc
    | hd :: tl -> sumList(tl, hd + acc)
 
let (|Empty|HasDelimiters|Normal|) (str : string) =
    if str="" then Empty else
    if str.StartsWith("//") then HasDelimiters
    else Normal

let split(str:string) = 
    List.ofArray(str.Split(',')) 
    |> List.map(fun x -> List.ofArray(x.Split[|'\n'|])) 
    |> List.collect(fun x -> x)
   
let parseNumbers numbers = numbers |> List.map(fun x -> System.Int32.Parse(x))

let filterOverThousand numbers = numbers |> List.filter(fun x -> x < 1000) 
   
let sumDelimited(delimited:string) = 
    let numbers = 
            delimited 
            |> split 
            |> parseNumbers
            |> filterOverThousand
    let negatives = numbers |> List.filter (fun number -> number < 0)
    match negatives.Length with
    | 0 -> sumList(numbers, 0)
    | _ -> failwith ("Negatives not allowed: " + (negatives |> List.map(fun x -> x.ToString()) |> String.concat(",") ))

let gatherDelimiters (x:string) = 
    x.Replace("][",",").Replace("]", "").Replace("[", "").Split[|','|] |> List.ofArray
    
let separateDelimitersAndValues (input : string) = 
    let pattern="^\/\/(?<delimiters>.*?)\\n(?<valuesString>.*)"
    let regexMatch = Regex.Match(input, pattern)  
    let delimiters = gatherDelimiters regexMatch.Groups.["delimiters"].Value
    let valuesString = regexMatch.Groups.["valuesString"].Value
    (delimiters,valuesString)  
    
let unifyDelimiters(input:string) =
    if input.[2] = '[' then 
        let delimiters, values  = separateDelimitersAndValues input
        List.fold (fun (acc:string) item -> acc.Replace(item, ",")) values delimiters
    else 
        let delimeter = input.Substring(2,1)
        let rest = input.Substring(4)
        rest.Replace(delimeter, ",")
  
let Add(input:string) =
    match input with
    | Empty -> 0
    | HasDelimiters -> unifyDelimiters input |> sumDelimited
    | _ -> sumDelimited input 
        
[<TestFixture>] 
type ``Given adding strings`` () =
    [<Test>]
    member x.``Adding empty strings is zero``()=
        Add "" |> should equal 0
    [<Test>]
    member x.``Single number returns itself``()=
        Add "1" |> should equal 1
    [<Test>]
    member x.``two numbers are summed``()=
        Add "1,2" |> should equal 3
    [<Test>]
    member x.``can sum any amount of numbers``()=
        Add "1,2,3" |> should equal 6
        Add "1,2,3,4" |> should equal 10
    [<Test>]
    member x.``numbers can be delimited with newline``()=
        Add "1,2\n3" |> should equal 6
    [<Test>]
    member x.``numbers can be delimited with given delimiter``()=
        Add "//;\n1;2" |> should equal 3
    [<Test; ExpectedException(typeof<System.Exception>, ExpectedMessage="Negatives not allowed: -1")>]
    member x.``negative numbers throw exception``()=
        (Add "-1,2") |> ignore
    [<Test; ExpectedException(typeof<System.Exception>, ExpectedMessage="Negatives not allowed: -1,-3")>]
    member x.``all negatives listed on error message``()=
        (Add "-1,2,-3") |> ignore
    [<Test>]
    member x.``ignore over 1000``()=
        Add "1000,2" |> should equal 2
    [<Test>]
    member x.``delimiter can have any length``()=
        Add "//[***]\n1***2***3" |> should equal 6
    [<Test>]
    member x.``can have several delimiters``()=
        Add "//[*][%]\n1*2%3" |> should equal 6
    [<Test>]
    member x.``can have several delimiters of any length``()=
        Add "//[***][%]\n1***2%3" |> should equal 6
