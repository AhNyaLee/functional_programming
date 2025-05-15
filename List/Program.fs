namespace WorkingWithLists

module ListOperations =
    let readList (n: int) =
        let rec readNumbers remaining accum =
            match remaining with
            | 0 -> List.rev accum
            | x when x > 0 ->
                printf "Введите число: "
                let newElem = System.Console.ReadLine() |> int
                readNumbers (remaining - 1) (newElem :: accum)
            | _ -> failwith "Ошибка в рекурсивной функции"

        match n with 
        | x when x < 0 -> failwith "Количество элементов не может быть отрицательным"
        | _ -> readNumbers n []

    let rec writeList list =
        match list with
        | [] -> ()
        | head :: tail -> 
            System.Console.WriteLine(head.ToString())
            writeList tail
    let rec reduceListWithCondition list (func: int -> int -> int) (condition: int -> bool) (accum: int) =
        match list with
        [] -> accum
        | head :: tail when condition head -> reduceListWithCondition tail func condition (func head accum)
        | head :: tail when (condition head) = false  -> reduceListWithCondition tail func condition accum
        | _ -> failwith "Непредвиденная ошибка"
    
    let theMostFrequentInList list =
        
        let rec elemFrequentsInList list accum_map = 
            match list with
            [] -> accum_map
            | head :: tail -> 
                if Map.containsKey head accum_map then
                    let currentCount = accum_map.[head]
                    elemFrequentsInList tail (Map.add head (currentCount + 1) accum_map)
                else
                    elemFrequentsInList tail (Map.add head 1 accum_map)

        let frequencyMap = elemFrequentsInList list Map.empty

        let mostFrequent = 
            frequencyMap |> Map.toSeq |> Seq.maxBy snd
        
        mostFrequent
    let countSquareElements lst =
        lst 
        |> List.filter (fun x -> List.exists (fun y -> y * y = x) lst) // Проверяем, есть ли элемент y, квадрат которого равен x
        |> List.length 
    //9
    // Функция для вычисления суммы цифр числа (без учёта знака)
    let sumOfDigits n =
        let rec loop acc x =
            if x = 0 then acc
            else loop (acc + x % 10) (x / 10)
        loop 0 (abs n)

// Функция для подсчёта количества делителей числа
    let countDivisors n =
        let absN = abs n
        if absN = 0 then 0
        else
            [1..absN] 
            |> List.filter (fun x -> absN % x = 0)
            |> List.length

// Функция для сортировки списков по заданным правилам
    let prepareLists (listA, listB, listC) =
      
        let sortedA = listA |> List.sortDescending
        
        // Сортировка списка B: по сумме цифр (возрастание), затем по модулю (убывание)
        let sortedB = 
            listB 
            |> List.sortWith (fun x y ->
                let sumX = sumOfDigits x
                let sumY = sumOfDigits y
                if sumX <> sumY then compare sumX sumY
                else compare (abs y) (abs x))
        
    // Сортировка списка C: по количеству делителей (убывание), затем по модулю (убывание)
        let sortedC = 
            listC 
            |> List.sortWith (fun x y ->
                let divX = countDivisors x
                let divY = countDivisors y
                if divX <> divY then compare divY divX
                else compare (abs y) (abs x))
        
        (sortedA, sortedB, sortedC)


    let createTriples listA listB listC =
        let (a, b, c) = prepareLists (listA, listB, listC)
        List.map3 (fun ai bi ci -> (ai, bi, ci)) a b c

    let readStrings n =
        let rec readItems acc remaining =
            if remaining <= 0 then acc |> List.rev
            else
                printf "Введите строку %d: " (n - remaining + 1)
                let item = readList 3
                readItems (item::acc) (remaining - 1)
        readItems [] n

    let sumEvenInList list =
         reduceListWithCondition list (+) (fun a -> a % 2 = 0) 0
 
    let countOddInList list =
         reduceListWithCondition list (fun a _ -> a + 1) (fun a -> a % 2 <> 0) 0
 
    let minInList list =
         reduceListWithCondition list min (fun a -> true) 10


type ChurchList<'T> = 
    | Nil 
    | Cons of 'T * ChurchList<'T>

module ChurchList =
    let cons head tail = Cons(head, tail)
    let empty = Nil

    let rec fold f acc = function
        | Nil -> acc
        | Cons(h, t) -> fold f (f acc h) t

    let length lst = fold (fun acc _ -> acc + 1) 0 lst
    let append a b = fold (fun acc x -> cons x acc) b a 
    let splitAt n lst =
        let rec loop n acc = function
            | Nil -> (acc, Nil)
            | Cons(h, t) as l -> 
                if n = 0 then (acc, l)
                else loop (n-1) (cons h acc) t
        loop n Nil lst


module Program =
    [<EntryPoint>]
    let main args =
        let numbers = ListOperations.readList 3
        printfn "Список: %A" numbers

        System.Console.WriteLine("Вывод элементов списка: ")
        ListOperations.writeList numbers

        let reducedList = ListOperations.reduceListWithCondition numbers (*) (fun a -> a % 2 = 0) 1
        System.Console.WriteLine("Свернутое значение списка: {0}", reducedList)
       // №4
        let minElem = ListOperations.minInList numbers
        System.Console.WriteLine("Минимальный элемент списка: {0}", minElem)

        let sumEven = ListOperations.sumEvenInList numbers
        System.Console.WriteLine("Сумма четных элемент списка: {0}", sumEven)

        let countOdd = ListOperations.sumEvenInList numbers
        System.Console.WriteLine("Количество нечетных элементов в списке: {0}", countOdd)

         // №5
        let most_frequent = ListOperations.theMostFrequentInList [1; 5; 1; -6; 1; 0; 7; 3; 3]
        System.Console.WriteLine("Элемент {0} в списке повторяется наибольшее число раз: {1}", (fst most_frequent), (snd most_frequent))
 
        let an = ListOperations.countSquareElements [2; 4; 16]
        System.Console.WriteLine("сколько элементов из него могут быть квадратом какого-то из элементов списка {0}",an )

        //9
        let listA = [10; 5; 3]
        let listB = [123; -45; 67]
        let listC = [6; 12; 4]
        let result = ListOperations.createTriples listA listB listC     
        System.Console.WriteLine("номер 9 {0}",result )

        0