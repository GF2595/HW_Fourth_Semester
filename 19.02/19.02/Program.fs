// Дополнительные сведения о F# см. на http://fsharp.net
// Дополнительную справку см. в проекте "Учебник по F#".


let rec factorial x = 
    if x = 1 then 1 else x * factorial (x - 1)

let rec fibonacci x = 
    if x = 1 then 1 else if x = 2 then 1 else fibonacci (x - 1) + fibonacci (x - 2)

let reverseList list =
    let rec reverseListInner result list = 
        match list with
        | head :: tale -> 
            let temp = head :: result 
            reverseListInner temp tale
        | [] -> result
    reverseListInner [] list

let buildList length = 
    let rec binaryPower i = 
        if i = 0 then 1 else 2 * binaryPower (i - 1)
    List.init length (fun i -> binaryPower i)

[<EntryPoint>]
let main argv = 
    let x = factorial (3)
    printfn "%d" x

    let list = buildList 10

    let reversedlist = reverseList list

    printfn "%A" argv
    0 // возвращение целочисленного кода выхода
