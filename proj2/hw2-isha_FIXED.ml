let rec get_match lst nonterm =
    match lst with
    [] -> []
    | a::rest ->
        match a with
        (b,c) when b = nonterm -> c::(get_match rest nonterm)
        | (b,c) -> get_match rest nonterm
;;


let convert_grammar gram1 = (* good! *)
    match gram1 with
    (a,b) -> a, (get_match b)
;;

type ('nonterminal, 'terminal) symbol =
 | N of 'nonterminal
 | T of 'terminal
;;

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal
;;

let rec parse_node lst =
    match lst with
        [] -> []
        | a::rest ->
            match a with
                Leaf l -> l::(parse_node rest)
                | Node (_,k) -> (parse_node k)@(parse_node rest)
;;

let rec parse_tree_leaves tree =  (* good! *)
    match tree with
    Node (_,lst) -> parse_node lst
    | Leaf l -> [l]
;;

(* make matcher *)
let rec rule_match (gram,rr) k fr =
match rr with
[] -> (false, fr)
| rule::rest -> 
    (match (list_match (gram,rule,List.length fr) k fr) with
    (false, _) -> rule_match (gram, rest) k fr
    | (true, frag) ->  (true, frag))

and list_match (gram,rl,ll) k fl =
if List.length rl > ll 
then (false, fl)
else

match rl with
[] -> k (true, fl) 
(* rule is a match *)

| term::trest ->
    (match term with
    T tt when (tt = List.hd fl) -> list_match (gram, trest, ll-1) k (List.tl fl)
    | T tt -> (false,fl)
    | N nn ->
        let subrules = snd (gram) nn in
        let wrap_k = function
        | (true, f) -> list_match (gram, trest, List.length f) k f
        | (false,ff) -> (false, ff) in

        (rule_match (gram,subrules) wrap_k fl))
;;

let make_matcher gram acceptor frag = 
let spec_accept (t_f, suffix) = 
match t_f with
true -> 
    (match (acceptor suffix) with
        Some x -> (true, suffix)
        | None -> (false, suffix))
| false -> (false, suffix) in

match gram with
(s,r) -> 
(match (fst (rule_match (gram, (r s)) spec_accept frag)) with
true -> acceptor (snd (rule_match (gram, (r s)) spec_accept frag))
| false -> None)
;;

let accept_end (tree, empty_list) =
match empty_list with
    | [] ->
        (match tree with
            | None -> (None, empty_list)
            | Some x -> (Some x, empty_list))
    | _ -> (None, empty_list)
;;

(* make parser *)
let rec rule_parse (gram,rr,tr) k (fr: string list) =

match rr with
[] -> (None, fr)
| rule::rest ->
    (match (list_parse (gram,rule,List.length fr, tr) k fr) with
        (None, _) -> rule_parse (gram, rest, tr) k fr
        | (Some x, f) -> (Some x, f)
    )

and list_parse (gram,rl,ll,trl) k (fl: string list) =

if List.length rl > ll 
then (None, fl)
else
match rl with
[] -> k (Some trl, fl)
(* rule is a match *)

| term::trest ->
    (match term with
    T tt when (tt = List.hd fl) -> list_parse (gram, trest, ll-1, (List.append trl [Leaf tt])) k (List.tl fl)
    | T tt -> (None, fl)
    | N nn -> 
        let subrules = snd (gram) nn in
        let wrap_k = function
        | (None, fl) -> (None, fl)
        | ((Some x),f) -> (list_parse (gram, trest, List.length f, (List.append trl [Node (nn, x)])) k f) in
        (rule_parse (gram,subrules,[]) wrap_k fl))
;;

let make_parser gram frag = 
match gram with
(s,r) -> match (fst (rule_parse (gram,(r s),[]) accept_end frag)) with
Some x  ->  Some (Node (s, x))
| _ -> None
;;

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num;;
  
let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]];;

let awksub_grammar = Expr, awksub_rules;;


let awksub_grammar_hw2 = convert_grammar awksub_grammar;;

let test_start_symbol = 
    fst awksub_grammar = fst awksub_grammar_hw2;;
let test_Expr = 
    (snd awksub_grammar_hw2) Expr = [   
                                        [T"("; N Expr; T")"];
                                        [N Num];
                                        [N Expr; N Binop; N Expr];
                                        [N Lvalue];
                                        [N Incrop; N Lvalue];
                                        [N Lvalue; N Incrop]
                                    ];;
let test_Lvalue =
    (snd awksub_grammar_hw2) Lvalue = [[T"$"; N Expr]];;

let test_Incrop =
    (snd awksub_grammar_hw2) Incrop = [[T"++"];[T"--"]];;

let test_Binop =
    (snd awksub_grammar_hw2) Binop = [[T"+"];[T"-"]];;

let test_Num =
    (snd awksub_grammar_hw2) Num = [[T"0"];[T"1"];[T"2"];[T"3"];[T"4"];[T"5"];[T"6"];[T"7"];[T"8"];[T"9"]];;

