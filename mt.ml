let rec even = function
    | 0 -> true
    | x -> odd(x-1)
and odd = function 
    | 0 -> false 
    | x-> even(x-1)

let rec reverse_list_helper reversed_evens L = function 
    | h::t -> (match even h with 
        | true -> (List.hd reversed_evens)::(reverse_list_helper (List.tl reversed_evens L))
        | false -> h::(reverse_list_helper t L)
    | [] -> []

let revHalf L = 
    let evens = List.filter (fun x -> even x) L in 
    let reversed_evens = List.rev evens in 
    reverse_list_helper reversed_evens L

