let subset a b = 
    List.for_all(fun x -> List.mem x b) a

let equal_sets a b = subset a b && subset b a

let set_intersection a b = 
    List.filter(fun x -> List.mem x b) a

let set_diff a b = 
    List.filter(fun x -> not (List.mem x b)) a

let set_union a b = 
    a @ (set_diff b a)

let rec computed_fixed_point eq f x =
    let point = f x in
    if eq point x then x else computed_fixed_point eq f point 

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec traverse remaining_rules reachable = match remaining_rules with 
    | h::t -> let reached_rules = List.filter(fun x -> let (left, right) = x in List.mem (N (left)) reachable) remaining_rules in 
      let newly_reached_symbols = List.concat (snd (List.split reached_rules)) in (match newly_reached_symbols with 
        | h::t -> traverse (set_diff remaining_rules reached_rules) newly_reached_symbols
        | _ -> set_diff remaining_rules reached_rules)
    | _ -> remaining_rules

let filter_reachable g = 
    let (start, rules) = g in 
    let remaining_rules = traverse rules [N (start)] in 
    (start, (set_diff rules remaining_rules))

