let accept_all x = Some x
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type sample_non_terminals =
| A | B | C

let sample_grammar = (A, 
    function 
        | A -> [[T 1; N B;]; [T 5]]
        | B -> [[T 2; N C]; [T 4]]
        | C -> [[T 3; N B; N A]])

let accept_all x = Some x

let frag = [1; 2; 3; 4; 5;]

let make_matcher_test = ((make_matcher sample_grammar accept_all frag) = Some [])

let make_parser_test = match (make_parser sample_grammar frag) with
    | Some tree when ((parse_tree_leaves tree) = frag) -> true
    | _ -> false
