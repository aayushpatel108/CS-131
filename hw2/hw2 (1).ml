type ('nonterminal, 'terminal) symbol = | N of 'nonterminal | T of 'terminal;;

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let convert_grammar gram1 =
    let rec search rules symbol = match rules with 
        | h::t -> let left, right = h in
            if left = symbol then right::(search t right)
            else (search t right)
        | _ -> []
    in let start, rules = gram1 in 
        start, fun symbol -> search rules symbol

let parse_tree_leaves tree = 
    let rec across tree_list  = match tree_list with 
        | h::t -> (down h)@(across t)
        | _ -> []
    and down tree  = match tree with 
        | Node (symbol, tree_list) ->  across tree_list
        | Leaf l -> [l] 
    in match tree with 
        | Leaf l -> [l]
        | Node (symbol, tree_list) -> across tree_list

let rec traverse_rule_list production_function cps frag = function 
    | first_rule::remaining_rules -> (match (traverse_rule production_function cps first_rule frag) with 
        | None -> traverse_rule_list production_function cps frag remaining_rules
        | result -> result)
    | _ -> None 
and traverse_rule production_function cps rule frag = match rule with  
    | (N nonterminal_symbol)::remaining_symbols -> 
        let new_rules_list = production_function nonterminal_symbol in
        let new_cps = traverse_rule production_function cps remaining_symbols in 
        traverse_rule_list production_function new_cps frag new_rules_list
    | (T terminal_symbol)::remaining_symbols -> 
        (match frag with 
            | start_frag::rest_frag when start_frag = terminal_symbol -> 
                traverse_rule production_function cps remaining_symbols rest_frag
            | _ -> None)
    | _ -> cps frag

let make_matcher gram accept frag = 
    let start, production_function = gram in 
    let rules_list = production_function start in 
        traverse_rule_list production_function accept frag rules_list

let rec across rule_list = function 
    | first_symbol::remaining_symbols -> let new_rule,node = (down rule_list first_symbol) in
            let next_call_result,tree_list = (across new_rule remaining_symbols) in (next_call_result, node::tree_list) 
    | _ -> (rule_list, []) 
and down rule_list = function
    | (N nonterminal_symbol) -> (match rule_list with 
        | first_rule::remaining_rules -> let rule_list,tree_list = across remaining_rules first_rule in 
                        (rule_list, Node (nonterminal_symbol, tree_list))
        | _ -> ([], Node (nonterminal_symbol, [])))
    | (T terminal_symbol) -> (match rule_list with 
        | first_rule::remaining_rules -> (rule_list, Leaf terminal_symbol)
        | _ -> ([], Leaf terminal_symbol))

let rec traverse_rule_list1 production_function cps frag = function 
    | current_rule::remaining_rules -> (match (traverse_rule1 production_function cps current_rule frag) with 
        | Some result -> Some (current_rule::result)
        | _ -> traverse_rule_list1 production_function cps frag remaining_rules)
    | _ -> None 
and traverse_rule1 production_function cps rule frag = match rule with  
    | (N nonterminal_symbol)::remaining_symbols -> 
        let new_rules_list = production_function nonterminal_symbol in
        let new_cps = traverse_rule1 production_function cps remaining_symbols in 
        traverse_rule_list1 production_function new_cps frag new_rules_list
    | (T terminal_symbol)::remaining_symbols -> 
        (match frag with 
            | start_frag::rest_frag when start_frag = terminal_symbol -> 
                traverse_rule1 production_function cps remaining_symbols rest_frag
            | _ -> None)
    | _ -> cps frag

let accept_empty_list = function
   | [] -> Some []
   | _ -> None

let make_parser gram frag = 
    let start, production_function = gram in 
    let rules_list = production_function start in 
    match (traverse_rule_list1 production_function accept_empty_list frag rules_list) with 
        | Some [] -> None 
        | Some rule_list -> let _, tree_list = (across rule_list [(N start)]) in 
            (match tree_list with 
                | root::tree -> Some root
                | _ -> None)
        | _ -> None

