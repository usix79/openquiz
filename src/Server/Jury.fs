module Jury

open System
open System.Text.RegularExpressions
open F23.StringSimilarity

let allowedCharacters = Regex ("[^\w\+\-\.]+", RegexOptions.Compiled)

let private normalizeWords (words: string array) =
    String.Concat(words).ToLower()

let private normalize txt =
    normalizeWords <| allowedCharacters.Split txt

let private allowedDistance (words: string list) =
    words
    |> List.fold (fun sum word -> sum + if word.Length > 4 then 1.0 else 0.0) 0.0

let private splitOnParts (answer : string) =

    let parseBracketsText (txt:string) =
        match txt.Split('/') with
        | res when res.Length = 1 -> [|txt; ""|]
        | res -> res
        |> List.ofSeq

    let mutable isInsideBrackets = false
    let mutable parts = Collections.Generic.List<string list>()
    let mutable txt = Text.StringBuilder()

    for ch in answer do
        match ch with
        | '[' when not isInsideBrackets ->
            if txt.Length > 0 then parts.Add [txt.ToString()]
            txt.Clear() |> ignore
            isInsideBrackets <- true
        | ']' when isInsideBrackets ->
            if txt.Length > 0 then parts.Add (parseBracketsText (txt.ToString()))
            txt.Clear() |> ignore
            isInsideBrackets <- false
        | _ -> txt.Append ch |> ignore

    if txt.Length > 0 then
        match isInsideBrackets with
        | true -> parts.Add (parseBracketsText (txt.ToString()))
        | false -> parts.Add [txt.ToString()]

    parts |> List.ofSeq

let rec private permutateParts (parts: string list list) : string list list =
    match parts with
    | [head] -> head |> List.map (fun s -> [s])
    | head::tail ->
        head |> List.collect (fun s -> tail |> permutateParts |> List.map (fun t -> s :: t))
    | [] -> []

let private allSubstitutions (answer: string) =
    splitOnParts answer
    |> permutateParts
    |> List.map String.Concat

let private allPermutations (words: string list) =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)

    if words.Length < 5 then
        permute words
    else
        [words] // skip permutation for 5 and more words (5! = 120 cases)

let private extractCorrectAnswers (answerTxt:string)=
    answerTxt.Split('\n')
    |> List.ofArray
    |> List.collect allSubstitutions
    |> List.map (allowedCharacters.Split >> List.ofArray)
    |> List.collect allPermutations
    |> List.map (fun words -> words |> Array.ofList |> normalizeWords, allowedDistance words)

let jury answer =
    let answers = extractCorrectAnswers answer
    let mutable knownVersions : Map<string,bool> = Map.empty
    let alg = Damerau()

    fun version ->
        let version = normalize version

        match knownVersions.TryFind version with
        | Some res -> res
        | None ->
            let res = answers |> List.exists (fun (aw, dist) -> alg.Distance(aw, version) <= dist)
            knownVersions <- knownVersions.Add (version,res)
            res