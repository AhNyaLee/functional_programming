module WorkWithNumbers

let rec cirfrusum n =
    if n = 0 then 0
    else (n % 10) + (cirfrusum (n / 10))

let sumCifr n =
    let rec sumCifrl n curSum =
        if n = 0 then curSum
        else
            let n1 = n / 10
            let cifr = n % 10
            let newSum = curSum + cifr
            sumCifrl n1 newSum
    sumCifrl n 0

let rec factcir n =
    if n = 1 then 1
    else n * (factcir (n - 1))

let rec factcifr n =
    let rec factCifri n factCifr =
        if n = 1 then factCifr
        else
            let n1 = n - 1
            let newfact = factCifr * n
            factCifri n1 newfact
    factCifri n 1

[<EntryPoint>]
let main (args: string[]) =
    let b = 5
    let a = cirfrusum b
    let c = sumCifr b
    let v = factcir b
    let h = factcifr b

    System.Console.WriteLine(a)

    System.Console.WriteLine(c)

    System.Console.WriteLine(v)

    System.Console.WriteLine(h)
0;;