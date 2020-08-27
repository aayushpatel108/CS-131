let search_nonterminal_symbol start_symbol production_function target_symbol = 
    match (production_function start_symbol) with 
    | first_rule::remaining_rules -> 
        (match (search_rule production_function target_symbol first_rule) with 
            | true -> true
            | false -> search_list_of_rules production_function target_symbol remaining_rules)
    | _ -> false
and rec search_rule production_function target_symbol = function 
    | (N nonterminal_symbol)::remaining_symbols -> 
        (match (search_nonterminal_symbol nonterminal_symbol production_function target_symbol) with 
            | true -> true 
            | false -> search_rule production_function target_symbol remaining_symbols)
    | (T terminal_symbol)::remaining_symbols when terminal_symbol = target_symbol -> true
    | _ -> false

let rec make_tree_helper path = match path with
    | path_hd::path_tl ->
        let symbol, rule = List.hd path in
        let path_remaining, children = (get_children List.tl path rule) in 
        path_remaining, Node (symbol, children)
and get_children path_remaining rule = match rule with
    | (N sym)::rule_tl ->
        let next_path_remaining, curr_tree  = make_tree_helper path_remaining in
        let next_next_path_remaining, siblings = (get_children next_path_remaining rule_tl) in
        next_next_path_remaining, curr_tree::siblings
    | (T sym)::rule_tl
        let next_path_remaining, siblings  = (get_children path_remaining rule_tl) in
        next_path_remaining, (Leaf sym)::siblings) 
    | _ -> path_remaining, [] 

let make_tree path_maker fragment = 
	let path = path_maker fragment in match path with
        | Some x -> 
            let in_path = List.rev x in
            Some (snd (make_tree_helper in_path))
        | _ -> None

let make_parser gram = 
    let start, production_function = gram in 
    let rules_list = production_function start in 
	let path_maker = parse_rules_list production_function rules_list start parse_acceptor [] in
	make_tree path_maker;;

