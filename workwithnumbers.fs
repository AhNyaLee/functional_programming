open System

type SolveQuadratic =
    None
    | Linear of float
    | Quadratic of float * float


let solveQuadr a b c =
        let D = b * b - 4. * a * c
        if a = 0. then
            if b = 0. then None
            else Linear(-c / b)
        else
            if D < 0. then None
            else Quadratic(( (-b + sqrt(D)) / (2. * a), (-b - sqrt(D)) / (2. * a) ))

let squareCircle r = 
    (System.Math.PI * r ** 2.0)

let volume_cylinder_through_superpos (r, h) =
    let square_cylinder_base = squareCircle r
    h * square_cylinder_base

let volume_cylinder_through_carry r h =
    let square_cylinder_base = squareCircle r
    h * square_cylinder_base

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
// №1
    printfn "Hello, World"

// №2
    System.Console.WriteLine("Введите коэффициенты квадратного уравения a, b, c:")
    let a = Double.Parse(System.Console.ReadLine())
    let b = Double.Parse(System.Console.ReadLine())
    let c = Double.Parse(System.Console.ReadLine())

    let roots = solveQuadr a b c
    match roots with
        None -> System.Console.WriteLine("Нет решений")
        | Linear(x) -> System.Console.WriteLine("Единственный корень: {0}", x)
        | Quadratic(x, y) -> System.Console.WriteLine("Корни: {0} {1}", x, y)

   // №3
    System.Console.WriteLine("Введите радиус и высоту цилиндра:")
    let r = Double.Parse(System.Console.ReadLine())
    let h = Double.Parse(System.Console.ReadLine())
    
    let volume_superpos = volume_cylinder_through_superpos (r, h)
    System.Console.WriteLine("(Суперпозиция) Объем цилиндра с радиусом основания {0} и высотой {1}: {2}", r, h, volume_superpos)

    let volume_carry = volume_cylinder_through_carry r h
    System.Console.WriteLine("(Каррирование) Объем цилиндра с радиусом основания {0} и высотой {1}: {2}", r, h, volume_carry)




    let b = 12
    let a = cirfrusum b
    let c = sumCifr b
    let v = factcir b
    let h = factcifr b


    System.Console.WriteLine(a)
    System.Console.WriteLine(c)
    System.Console.WriteLine(v)
    System.Console.WriteLine(h)

    
    0
