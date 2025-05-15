
let shiftLeftThree lst =
    let length = List.length lst
    if length <= 3 then lst @ lst |> List.take length
    else
        let shift = 3 % length
        List.append (List.skip shift lst) (List.take shift lst)

// Метод 2: Хвостовая рекурсия
let shiftLeftThreeRec lst =
    let rec shiftOnce acc = function
        | [] -> acc
        | x::xs -> shiftOnce (acc @ [x]) xs  // Исправлена логика сдвига
    
    let shiftSize = 
        let len = List.length lst
        if len = 0 then 0 else 3 % len
    
    [1..shiftSize] 
    |> List.fold (fun s _ -> shiftOnce [] s |> List.rev) lst 
    |> List.rev

// Задача 2: Элементы между первым и вторым максимальным (исправлено)

// Метод 2: Рекурсивный поиск
let betweenFirstSecondMaxRec lst =
    let rec findIndices idx first second maxVal = function
        | [] -> (first, second)
        | x::xs when x = maxVal ->
            match first with
            | -1 -> findIndices (idx+1) idx second maxVal xs
            | _ when second = -1 -> findIndices (idx+1) first idx maxVal xs
            | _ -> findIndices (idx+1) first second maxVal xs
        | _::xs -> findIndices (idx+1) first second maxVal xs
    
    if List.length lst < 2 then []
    else
        let maxVal = List.max lst
        let first, second = findIndices 0 -1 -1 maxVal lst
        if second = -1 then []
        else
            lst
            |> List.skip (first + 1)
            |> List.take (second - first - 1)
// Задача 2: Элементы между первым и вторым максимальным

// Метод 1: Использование стандартных функций
let betweenFirstSecondMax lst =
    let maxVal = List.max lst
    let indices = lst |> List.indexed |> List.filter (snd >> (=) maxVal) |> List.map fst
    if List.length indices < 2 then []
    else
        let first, second = List.item 0 indices, List.item 1 indices
        lst.[first+1..second-1]


// Задача 3: Количество элементов между первым и последним минимальным

// Метод 1: Использование стандартных функций
let countBetweenMinElements lst =
    if List.isEmpty lst then 0
    else
        let minVal = List.min lst
        let first = List.findIndex ((=) minVal) lst
        let last = List.findIndexBack ((=) minVal) lst
        max 0 (last - first - 1)

// Метод 2: Рекурсивный подсчёт
let countBetweenMinElementsRec lst =
    let rec findMinIndices idx currentMin first last = function
        | [] -> (first, last)
        | x::xs ->
            let newMin, newFirst, newLast =
                if x < currentMin then (x, idx, idx)
                elif x = currentMin then (currentMin, first, idx)
                else (currentMin, first, last)
            findMinIndices (idx+1) newMin newFirst newLast xs
    if List.isEmpty lst then 0
    else
        let first, last = findMinIndices 0 System.Int32.MaxValue -1 -1 lst
        if first = -1 || last = -1 then 0
        else max 0 (last - first - 1)

// Задача 4: Сначала положительные, затем отрицательные

// Метод 1: Фильтрация
let separatePositiveNegative lst =
    List.filter (fun x -> x > 0) lst @ List.filter (fun x -> x <= 0) lst

// Метод 2: Рекурсивный сбор
let separatePositiveNegativeRec lst =
    let rec collect pos neg = function
        | [] -> List.rev pos @ List.rev neg
        | x::xs ->
            if x > 0 then collect (x::pos) neg xs
            else collect pos (x::neg) xs
    collect [] [] lst

// Задача 5: Среднее непростых элементов, больших среднего простых

// Вспомогательная функция проверки на простое число
let isPrime n =
    if n <= 1 then false
    else
        let sqrtN = int (sqrt (float n))
        { 2 .. sqrtN } |> Seq.forall (fun i -> n % i <> 0)

// Метод 1: Использование стандартных функций
let complexAverage lst =
    let primes = List.filter isPrime lst
    if List.isEmpty primes then 0.0
    else
        let avgPrimes = List.averageBy float primes
        lst 
        |> List.filter (fun x -> not (isPrime x) && float x > avgPrimes)
        |> function 
            | [] -> 0.0 
            | nonPrimes -> List.averageBy float nonPrimes

// Метод 2: Рекурсивный подсчёт
let complexAverageRec lst =
    let rec isPrimeRec n i =
        if i * i > n then true
        elif n % i = 0 then false
        else isPrimeRec n (i + 1)
    
    let isPrime n = if n <= 1 then false else isPrimeRec n 2
    
    let rec sumCount f sum cnt = function
        | [] -> (sum, cnt)
        | x::xs ->
            if f x then sumCount f (sum + x) (cnt + 1) xs
            else sumCount f sum cnt xs
    
    let (primeSum, primeCnt) = sumCount isPrime 0 0 lst
    let avgPrimes = if primeCnt = 0 then 0.0 else float primeSum / float primeCnt
    
    let (nonPrimeSum, nonPrimeCnt) = 
        sumCount (fun x -> not (isPrime x) && float x > avgPrimes) 0 0 lst
    
    if nonPrimeCnt = 0 then 0.0 else float nonPrimeSum / float nonPrimeCnt

[<EntryPoint>]
let main args =
    let testList = [1; 2; 3; 4; 5]
    printfn "Циклический сдвиг влево на три позиции: %A" (shiftLeftThree testList)
    printfn "Рекурсивный сдвиг влево на три позиции: %A" (shiftLeftThreeRec testList)

    printfn "Элементы между первыми двумя максимумами: %A" (betweenFirstSecondMax [1;5;3;5;2])
    printfn "Рекурсивный поиск между максимумами: %A" (betweenFirstSecondMaxRec [1;5;3;5;2])

    printfn "Количество элементов между минимумами: %d" (countBetweenMinElements [1;3;0;4;0;5])
    printfn "Рекурсивный подсчёт между минимумами: %d" (countBetweenMinElementsRec [1;3;0;4;0;5])

    printfn "Разделение положительных и отрицательных: %A" (separatePositiveNegative [1; -2; 3; -4; 5])
    printfn "Рекурсивное разделение: %A" (separatePositiveNegativeRec [1; -2; 3; -4; 5])

    printfn "Среднее непростых элементов: %f" (complexAverage [4;5;6;7;8])
    printfn "Рекурсивное среднее непростых: %f" (complexAverageRec [4;5;6;7;8])
    
    0