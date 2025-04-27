namespace WorkingWithLists

open System
// Определение списка Черча
type ChurchList<'T> = 
    | Nil 
    | Cons of 'T * ChurchList<'T>

// Функция для чтения списка с клавиатуры
let readChurchList n =
    let rec readItems remainingItems acc =
        if remainingItems <= 0 then
            acc
        else
            printf "Введите элемент %d: " (n - remainingItems + 1)
            let item = System.Console.ReadLine()
            readItems (remainingItems - 1) (Cons(item, acc))
    
    readItems n Nil |> List.rev  // Переворачиваем список для правильного порядка

// Функция для преобразования списка Черча в обычный список F#
let churchListToRegularList (churchList: ChurchList<'T>) =
    let rec convert cl acc =
        match cl with
        | Nil -> acc |> List.rev
        | Cons(head, tail) -> convert tail (head::acc)
    convert churchList []

// Пример использования
[<EntryPoint>]
let main argv =
    printfn "Введите количество элементов:"
    let n = System.Console.ReadLine() |> int
    
    let churchList = readChurchList n
    printfn "Список Черча: %A" churchList
    
    let regularList = churchListToRegularList churchList
    printfn "Обычный список: %A" regularList
    
    0 // возвращаем код выхода