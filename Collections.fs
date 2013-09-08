[<AutoOpen>]
module Collections

module List =
    let rec phold f acc = function
        | [] -> acc
        | x::xs -> phold f (f acc x xs) xs

    let rec last = function
        | [] -> failwith "Empty list"
        | [x] -> x
        | _::xs -> last xs

    let dropLast xs =
        (xs, (false, []))
        ||> List.foldBack (fun v -> function
            | false, acc -> true, acc
            | _, acc -> true, v :: acc
        )
        |> snd

module Seq =
    let repeat xs =
        let xs = Seq.cache xs
        seq {
            while true do
                yield! xs
        }