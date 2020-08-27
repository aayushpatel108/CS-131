let rec subset a b  = match a with 
| [] -> true
| head::tail -> if List.mem head b then subset tail b
        else false
;;

let rec equal_sets a b = (subset b a ) && (subset a b )
;;

let set_union a b = a@b
;;

let rec set_intersection a b = match a with 
| [] -> []
| head::tail -> let c = set_intersection tail b in
    if List.mem head b then head::c
    else c
;;


let set_diff a b = List.filter (fun x -> not(List.mem x b )) a
;;


let rec computed_fixed_point eq f x = 
  if not (eq (f x) x) 
then computed_fixed_point eq f (f x )
 else x;;





type ('nonterminal, 'terminal) symbol = 
  | N of 'nonterminal
  | T of 'terminal;;

let rec nonTerRig r = match r with
  | [] -> []
  | head::tail -> match head with 
    | N head -> head::(nonTerRig tail)
    | T head -> nonTerRig tail;;


let rec el l element = match l with 
  | head::tail -> if head = element then 
    true
  else 
  el tail element
  | [] -> false;;


let rec getProperTerms rn rul = match rul with
  | rntup::rtup -> if (el rn (fst rntup) ) then 
let i = snd rntup in
let ii = nonTerRig i in
let iii = set_union rn ii in
getProperTerms iii rtup
  else getProperTerms rn rtup
  | [] -> rn ;;

let rec getNonTer c r = 
  let i = getProperTerms c r in
  let ii = getProperTerms i r in
  let z = set_union ii i in
  let iii = getProperTerms c r in
  let iiii = getProperTerms iii r in
  let y = set_intersection iiii iii in
  if equal_sets y z then i
  else getNonTer ii r 
;;

let filter_reachable g = match g with
  |  (v,w) -> (v, List.filter (fun x -> el (getNonTer [v] w) (fst x)) w)
;;



