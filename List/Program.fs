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

module Program =
    [<EntryPoint>]
    let main args =
        let numbers = ListOperations.readList 3
        printfn "Список: %A" numbers

        System.Console.WriteLine("Вывод элементов списка: ")
        ListOperations.writeList numbers

        let reducedList = ListOperations.reduceListWithCondition [1; 6; 8; 10] (*) (fun a -> a % 2 = 0) 1
        System.Console.WriteLine("Свернутое значение списка: {0}", reducedList)
        0