module stringcalc
open NUnit.Framework
open FsUnit
open System.Text.RegularExpressions

let rec sumList(list, acc) =
    match list with
    | [] -> acc
    | hd :: tl -> sumList(tl, System.Int32.Parse(hd) + acc)
 
let (|Empty|Delimiter|De|) (str : string) =
    if str="" then Empty else
    if str.StartsWith("//") then Delimiter
    else De

let sumUsingDelimiter(delimited:string) = 
    let split = 
            List.ofArray(delimited.Split(','))
            |> List.map(fun x -> List.ofArray(x.Split[|'\n'|])) 
            |> List.collect(fun x -> x)
            |> List.filter(fun x -> System.Int32.Parse(x) < 1000)
    let negatives = split |> List.filter (fun str -> str.[0] = '-')
    match negatives.Length with
    | 0 -> sumList(split, 0)
    | _ -> failwith ("Negatives not allowed: " + (negatives |> String.concat ","))

let parseDelimiter(x:string) =
    if x.[2] = '[' then 
        let pattern="^\/\/(?<delimiters>.*?)\\n(?<valuesString>.*)"
        let regexMatch = Regex.Match(x, pattern)
        let delimiters = regexMatch.Groups.["delimiters"].Value.Replace("][",",").Replace("]", "").Replace("[", "").Split[|','|]
        let valuesString = regexMatch.Groups.["valuesString"].Value
        let deli = List.ofArray delimiters
        List.fold (fun (acc:string) item -> acc.Replace(item, ",")) valuesString deli
    else 
        let delimeter = x.Substring(2,1)
        let rest = x.Substring(4)
        rest.Replace(delimeter, ",")
  
let AddString(x:string) =
    match x with
    | Empty -> 0
    | Delimiter -> parseDelimiter x |> sumUsingDelimiter
    | _ -> sumUsingDelimiter x 
        
[<TestFixture>] 
type ``Given adding strings`` () =
    [<Test>]
    member x.``Adding empty strings is zero``()=
        AddString "" |> should equal 0
    [<Test>]
    member x.``Single number returns itself``()=
        AddString "1" |> should equal 1
    [<Test>]
    member x.``two numbers are summed``()=
        AddString "1,2" |> should equal 3
    [<Test>]
    member x.``can sum any amount of numbers``()=
        AddString "1,2,3" |> should equal 6
        AddString "1,2,3,4" |> should equal 10
    [<Test>]
    member x.``numbers can be delimited with newline``()=
        AddString "1,2\n3" |> should equal 6
    [<Test>]
    member x.``numbers can be delimited with given delimiter``()=
        AddString "//;\n1;2" |> should equal 3
    [<Test; ExpectedException(typeof<System.Exception>, ExpectedMessage="Negatives not allowed: -1")>]
    member x.``negative numbers throw exception``()=
        (AddString "-1,2") |> ignore
    [<Test; ExpectedException(typeof<System.Exception>, ExpectedMessage="Negatives not allowed: -1,-3")>]
    member x.``all negatives listed on error message``()=
        (AddString "-1,2,-3") |> ignore
    [<Test>]
    member x.``ignore over 1000``()=
        AddString "1000,2" |> should equal 2
    [<Test>]
    member x.``delimiter can have any length``()=
        AddString "//[***]\n1***2***3" |> should equal 6
    [<Test>]
    member x.``can have several delimiters``()=
        AddString "//[*][%]\n1*2%3" |> should equal 6
    [<Test>]
    member x.``can have several delimiters of any length``()=
        AddString "//[***][%]\n1***2%3" |> should equal 6
