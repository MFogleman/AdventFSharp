#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
// include Fake modules, see Fake modules section

open System
open System.IO

let getFile = File.ReadAllLines "day01_01_input.txt"
let comps p x = p + x = 2020
let multiply a b = a * b

let threeEqual2020 a b c = a + b + c = 2020



// for (let i = 0; i < arr.length-2; i += 1) {
//     for (let j = i+1; j < arr.length-1; j += 1) {
//         for (let k = j + 1; k < arr.length; k += 1) {
//             if (arr[i]+arr[k]+arr[j] === 2020) {
//                 console.log('done', arr[i], arr[k], arr[j])
//             } 
//         }
//     }
// }
// find 2 numbers in nums that equal 2020

let maybePrint i j k =
    if i + j + k = 2020 then 
        printfn "%d %d %d" i j k
    
let rec findDigitsToMatch2020 nums =    
    match nums with
    | a::b::c::xs -> 
        List.iter(fun i -> 
            List.iter( fun j -> 
                List.iter ( fun k -> 
                    maybePrint i j k
                ) nums
            ) nums 
        ) nums
        
    | _ -> printfn "not found"
//84035952


getFile 
    |> Array.toList
    |> List.map int
    |> findDigitsToMatch2020
    // |> List.reduce multiply
    |> Console.WriteLine

