// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from jasbir kaur"

// List of earnings
let earnings = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

// Display list of salaries
printfn "List of salaries: %A" earnings

// Filter higher wages (> $100,000)
let higherWages = earnings |> List.filter (fun earning -> earning > 100000)
printfn "Higher wages: %A" higherWages

// Tax calculation function
let taxCal earning =
    match earning with
    | e when e <= 49020 -> float e * 0.15
    | e when e <= 98040 -> float 49020 * 0.15 + float (e - 49020) * 0.205
    | e when e <= 151978 -> float 49020 * 0.15 + float (98040 - 49020) * 0.205 + float (e - 98040) * 0.26
    | e when e <= 216511 -> float 49020 * 0.15 + float (98040 - 49020) * 0.205 + float (151978 - 98040) * 0.26 + float (e - 151978) * 0.29
    | _ -> float 49020 * 0.15 + float (98040 - 49020) * 0.205 + float (151978 - 98040) * 0.26 + float (216511 - 151978) * 0.29 + float (earning - 216511) * 0.33

let taxes = earnings |> List.map taxCal
printfn "Taxes for all earnings: %A" taxes

// Adjust earnings less than $49,020 by adding $20,000
let adjustedEarnings = earnings |> List.map (fun earning -> if earning < 49020 then earning + 20000 else earning)
printfn "Adjusted earnings: %A" adjustedEarnings

// Filter earnings between $50,000 and $100,000 and sum them
let sum = earnings |> List.filter (fun earning -> earning >= 50000 && earning <= 100000) |> List.sum
printfn "Sum of earnings between $50,000 and $100,000: %d" sum

// Tail recursion to calculate the sum of all multiples of 3 up to a given number
let rec sumMultiplesOf3 n total =
    match n with
    | 0 -> total
    | _ -> sumMultiplesOf3 (n - 3) (total + n)

printfn "Sum of multiples of 3 up to 27: %d" (sumMultiplesOf3 27 0)