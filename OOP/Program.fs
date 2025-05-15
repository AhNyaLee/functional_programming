// Определение функтора Box
type Box<'T> = 
    | Value of 'T  // Контейнер для значения

    // Метод Map реализует функтор
    member this.Map(transform: 'T -> 'U) =
        match this with
        | Value x -> Value(transform x)  // Применяем функцию к содержимому

// Создаем экземпляр Box с числом 5
let originalBox = Value 5

// Преобразования
let modifiedBox = 
    originalBox
        .Map(fun x -> x + 3)    // 5 + 3 = 8
        .Map(fun x -> x * 2)    // 8 * 2 = 16

// Выводим результат
printfn "Исходный Box: %A" originalBox
printfn "Преобразованный Box: %A" modifiedBox

// Распаковываем значение для проверки
match modifiedBox with
| Value v -> printfn "Результат: %d" v 