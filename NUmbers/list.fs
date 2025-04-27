namespace WorkingWithLists
open System

// Определение списка Черча
type ChurchList<'T> = 
    | Nil 
    | Cons of 'T * ChurchList<'T>

// Функция для реверса списка Черча
let rec reverseChurchList list =
    let rec reverse acc = function
        | Nil -> acc
        | Cons(head, tail) -> reverse (Cons(head, acc)) tail
    reverse Nil list

// Функция для чтения списка с клавиатуры
let readChurchList n =
    let rec readItems remainingItems acc =
        if remainingItems <= 0 then
            acc |> reverseChurchList  // Используем кастомный реверс
        else
            printf "Введите элемент %d: " (n - remainingItems + 1)
            let item = Console.ReadLine()
            readItems (remainingItems - 1) (Cons(item, acc))
    
    readItems n Nil

// Функция для преобразования списка Черча в обычный список F#
let churchListToRegularList churchList =
    let rec convert cl acc =
        match cl with
        | Nil -> acc |> List.rev
        | Cons(head, tail) -> convert tail (head::acc)
    convert churchList []

[<EntryPoint>]
let main (args: string[]) =

    printfn "Введите количество элементов:"
    let n = Console.ReadLine() |> int

    let churchList = readChurchList n
    printfn "Список Черча: %A" churchList

    let regularList = churchListToRegularList churchList
    printfn "Обычный список: %A" regularList

    0 // Код возврата