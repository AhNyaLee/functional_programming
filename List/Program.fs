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

    let sumEvenInList list =
         reduceListWithCondition list (+) (fun a -> a % 2 = 0) 0
 
    let countOddInList list =
         reduceListWithCondition list (fun a _ -> a + 1) (fun a -> a % 2 <> 0) 0
 
    let minInList list =
         reduceListWithCondition list min (fun a -> true) 10


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
 


        0