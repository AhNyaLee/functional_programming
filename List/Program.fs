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

module Program =
    [<EntryPoint>]
    let main args =
        let numbers = ListOperations.readList 3
        printfn "Список: %A" numbers
        0