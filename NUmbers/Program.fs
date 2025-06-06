﻿open System

type SolveQuadratic =
    None
    | Linear of float
    | Quadratic of float * float

let rec gcd a b =
    if b = 0 then a
    else gcd b (a % b)

let isPrime n =
    if n <= 1 then false
    else
        let sqrtN = sqrt (float n) |> int
        seq { 2 .. sqrtN } |> Seq.forall (fun i -> n % i <> 0)

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
    if n = 0 then 1
    else n * (factcir (n - 1))

let rec factcifr n =
    let rec factCifri n factCifr =
        if n = 1 then factCifr
        else
            let n1 = n - 1
            let newfact = factCifr * n
            factCifri n1 newfact
    factCifri n 1

let chooseFunction (digitSum: bool) =
        match digitSum with
        true -> cirfrusum 
        | false -> factcifr

let rec bypassDigits (num: int) (func: int -> int -> int) (accum: int) : int =
        match num with
        0 -> accum
        | _ -> bypassDigits (int num / 10) func (func (num % 10) accum) 

let rec bypassDigitsWithCondition (num: int) (twoArgFunc: int -> int -> int) (accum: int) (condition: int -> bool) : int =
    match num with
    0 -> accum
    | _ when (condition (num % 10)) = true -> bypassDigitsWithCondition (num / 10) twoArgFunc (twoArgFunc (num % 10) accum) condition
    | _ -> bypassDigitsWithCondition (num / 10) twoArgFunc accum condition



let favLang (lang: string) : unit =
     let result =
         match lang with
             "F#" | "Prolog" -> "ты подлиза"
             | "java" -> "интересный выбор"
             | "python" -> "крутой"
             | _ -> "ясно"
 
     result |> System.Console.WriteLine

let rec GCD (a: int, b: int) : int =
    match b with
    0 -> a
    | _ -> GCD (b, a % b)

let rec bypassMutuallyPrimeComponentsInNumber (current: int) (num: int) (func: int -> int -> int) (accum: int) : int =
     match current with
        x when x >= num -> accum
        | x when GCD(num, x) = 1 -> bypassMutuallyPrimeComponentsInNumber (current+1) num func (func current accum)
        | _ -> bypassMutuallyPrimeComponentsInNumber (current+1) num func accum
 

let EulerFunction (num: int) : int =
  bypassMutuallyPrimeComponentsInNumber 1 num (fun x acc -> acc + 1) 0
let rec bypassMutuallyPrimeWithCondition (current: int) (num: int) (func: int -> int -> int) (accum: int) (condition: int -> bool) =
    match current with
       x when x >= num -> accum
       | x when GCD (num, x) = 1 && condition x -> 
       bypassMutuallyPrimeWithCondition (current + 1) num func (func accum current) condition
       | _ -> bypassMutuallyPrimeWithCondition (current + 1) num func accum condition


// Метод 1
let sumNonPrimeDivisors n =
    let absN = abs n
    let divisors = [1..absN] |> List.filter (fun x -> absN % x = 0)
    divisors
    |> List.filter (fun d -> not (isPrime d))
    |> List.sum

// Метод 2
let countDigitsLessThan3 n =
    let absN = abs n
    absN.ToString().ToCharArray()
    |> Array.map (fun c -> int c - int '0')
    |> Array.filter (fun d -> d < 3)
    |> Array.length

let sumPrimeDigits n =
    let digits = 
        abs n
        |> string
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.toList
    digits
    |> List.filter (fun d -> List.contains d [2;3;5;7])
    |> List.sum

// Метод 3
let method3Count n =
    let absN = abs n
    let sumPrime = sumPrimeDigits absN
    [1..absN]
    |> List.filter (fun k ->
        (absN % k <> 0) &&         
        (gcd k absN > 1) &&        
        (gcd k sumPrime = 1)       
    )
    |> List.length
let chooseMethod (func_num: int, num: int) : unit =
    match func_num with
     1 -> sumNonPrimeDivisors num |> Console.WriteLine
     | 2 -> countDigitsLessThan3 num |> Console.WriteLine
     | 3 -> method3Count num |> Console.WriteLine
     | _ -> Console.WriteLine("Неверный номер")

[<EntryPoint>]
let main (args: string[]) =
// №1
    printfn "Hello, World"

// №2
    System.Console.WriteLine("Номер 2: Введите коэффициенты квадратного уравения a, b, c:")
    let a = Double.Parse(System.Console.ReadLine())
    let b = Double.Parse(System.Console.ReadLine())
    let c = Double.Parse(System.Console.ReadLine())

    let roots = solveQuadr a b c
    match roots with
        None -> System.Console.WriteLine("Нет действительных корней ")
        | Linear(x) -> System.Console.WriteLine("Единственный корень: {0}", x)
        | Quadratic(x, y) -> System.Console.WriteLine("Корни: {0} {1}", x, y)

   // №3
    System.Console.WriteLine("Номер 3: Введите радиус и высоту цилиндра:")
    let r = Double.Parse(System.Console.ReadLine())
    let h = Double.Parse(System.Console.ReadLine())
    
    let volume_superpos = volume_cylinder_through_superpos (r, h)
    System.Console.WriteLine("(Суперпозиция) Объем цилиндра с радиусом основания {0} и высотой {1}: {2}", r, h, volume_superpos)

    let volume_carry = volume_cylinder_through_carry r h
    System.Console.WriteLine("(Каррирование) Объем цилиндра с радиусом основания {0} и высотой {1}: {2}", r, h, volume_carry)

  //№4-5
    let b = 12
    let a = cirfrusum b
    let c = sumCifr b
    let v = factcir b
    let h = factcifr b


    System.Console.WriteLine(a)
    System.Console.WriteLine(c)
    System.Console.WriteLine(v)
    System.Console.WriteLine(h)


    //№6
    let factor = chooseFunction false
    Console.WriteLine("Результат: {0}", (factor 12))
    Console.WriteLine("Результат: {0}", (factor 12))
    let factor1 = chooseFunction true
    Console.WriteLine("Результат: {0}", (factor1 12))
    Console.WriteLine("Результат: {0}", (factor1  12))

    //№7-8
    let min_function = fun a b -> if a < b then a else b
    let min_digit =bypassDigits 1234 min_function 10
    System.Console.WriteLine("Минимальная цифра числа: {0}", min_digit)

    let max_function = fun a b -> if a > b then a else b
    let max_digit = bypassDigits 1234 max_function 0
    System.Console.WriteLine("Максимальная цифра числа: {0}", max_digit)

    let plus = fun a b -> a + b
    let plus_digits = bypassDigits 1234 plus 0
    System.Console.WriteLine("Сумма цифр числа: {0}", plus_digits)

    let mult = fun a b -> a * b
    let mult_digits = bypassDigits 1234 mult 1
    System.Console.WriteLine("Произведение цифр числа: {0}", mult_digits)

    //№9-10
    let min_function = fun a b -> if a < b then a else b
    let evenCondition = fun a -> if a % 2 = 0 then true else false
    let min_digit = bypassDigitsWithCondition 1234 min_function 10 evenCondition
    System.Console.WriteLine("Минимальная четная цифра числа: {0}", min_digit)

    let max_function = fun a b -> if a > b then a else b
    let oddCondition = fun a -> if a % 2 <> 0 then true else false
    let max_digit = bypassDigitsWithCondition 1234 max_function 0 oddCondition
    System.Console.WriteLine("Максимальная нечетная цифра числа: {0}", max_digit)

    let plus = fun a b -> a + b
    let notOne = fun a -> if a <> 1 then true else false
    let plus_digits = bypassDigitsWithCondition 1234 plus 0 notOne
    System.Console.WriteLine("Сумма цифр числа, которые не равны 1: {0}", plus_digits)

    let mult = fun a b -> a * b
    let notThree = fun a -> if a <> 3 then true else false
    let mult_digits = bypassDigitsWithCondition 1234 mult 1 notThree
    System.Console.WriteLine("Произведение цифр числа, которые не равны 3: {0}", mult_digits)

    //№11-12
    System.Console.Write("Какой Ваш любимый язык: ")
    let lang_choice = System.Console.ReadLine()
    favLang lang_choice
    favLang "java"
    favLang "С++"

    //№13   
    let res = bypassMutuallyPrimeComponentsInNumber 1 10 (fun a b -> a + b) 0
    System.Console.WriteLine("{0}", res)

    // №14
    let num = Console.ReadLine()
    let res =EulerFunction (int num)
    System.Console.WriteLine("Функция Эйлера от {0} есть {1}", num, res)
    
    // № 15
    let num = 15
    let res = bypassMutuallyPrimeWithCondition 1 num (+) 0 (fun a -> a % 2 <> 0)
    System.Console.WriteLine("Сумма нечетных взаимно-простых чисел от 1 до {0} есть {1}", num, res)
  
    //16  
    let anser =  sumNonPrimeDivisors 8
    System.Console.WriteLine("Cуммa непростых делителей числа {0}",anser )
    let anser1 =  countDigitsLessThan3 123
    System.Console.WriteLine("Количество цифр числа, меньших 3 -{0}",anser1 )
    let anser2 =  method3Count 12
    System.Console.WriteLine("Количество чисел, не являющихся делителями исходного числа, не взамно простых с ним и взаимно простых с суммой простых цифр этого числа {0}",anser2 )
    
    //20
    Console.Write("Введите номер метода: ")
    let func_num = Console.ReadLine() |> int
    Console.Write("Введите число: ")
    let num = Console.ReadLine() |> int
    chooseMethod (func_num, num)
    
    0   