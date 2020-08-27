let my_subset_test7 = subset [] []
let my_subset_test0 = subset [] [1;2]
let my_subset_test1 = subset [1;1;1] [1]
let my_subset_test2 = not (subset [1;2] [2;3;4])
let my_subset_test3 = subset [1;2] [1;2;3;4]
let my_subset_test4 = subset [1;2;3] [2;1;3]
let my_subset_test5 = subset [1;2] [2;1;3]
let my_subset_test6 = not (subset [1;2;3] [1])
let my_subset_test8 = subset [] [1;2;3]
let my_subset_test9 = subset [1;2;2] [1;2;3]
let my_subset_test10 = not (subset [1] [])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not (equal_sets [1] [])
let my_equal_sets_test2 = equal_sets [1;1;1] [1]
let my_equal_sets_test3 =  equal_sets [1;2;3] [3;2;1]
let my_equal_sets_test4 = not (equal_sets [1;2;3] [1])
let my_equal_sets_test5 = not (equal_sets [1] [])
let my_equal_sets_test6 = equal_sets [1;2] [1;1;2]
let my_equal_sets_test7 = not (equal_sets [1;2;2] [2;1;3])
let my_equal_sets_test8 = equal_sets [1] [1]
let my_equal_sets_test9 = not (equal_sets [1;2;3] [4;2;3])

let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [1;2;3] []) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [1] []) [1]
let my_set_union_test3 = equal_sets (set_union [1;2;3] [3;2;1]) [1;2;3]
let my_set_union_test4 = equal_sets (set_union [] [1;2;2]) [1;2]
let my_set_union_test5 = equal_sets (set_union [1;2] [3;4]) [1;2;3;4]
let my_set_union_test6 = equal_sets (set_union [1;2;3] [4;3;2]) [1;2;3;4]
let my_set_union_test7 = equal_sets (set_union [] [1]) [1]
let my_set_union_test8 = equal_sets (set_union [1;2;3] [1;2;3]) [1;2;3]

let my_set_intersection_test0 = equal_sets (set_intersection [] []) []
let my_set_intersection_test1 = equal_sets (set_intersection [1;2;3] [2;4]) [2]
let my_set_intersection_test2 = equal_sets (set_intersection [1;2;3] []) []
let my_set_intersection_test3 = equal_sets (set_intersection [1;1;1] [2;3;4]) []
let my_set_intersection_test4 = equal_sets (set_intersection [] [1]) []
let my_set_intersection_test5 = equal_sets (set_intersection [1;2;3;4] [3;4;5]) [3;4]
let my_set_intersection_test6 = equal_sets (set_intersection [1;2] [2;3]) [2]
let my_set_intersection_test7 = equal_sets (set_intersection [2;2] [2;2;2;2]) [2]
let my_set_intersection_test8 = equal_sets (set_intersection [3;4] [1;2;3]) [3]

let my_set_diff_test0 = equal_sets (set_diff [] []) []
let my_set_diff_test1 = equal_sets (set_diff [1;2;3] []) [1;2;3]
let my_set_diff_test2 = equal_sets (set_diff [1;2;3;4] [4;5;6;7]) [1;2;3]
let my_set_diff_test3 = equal_sets (set_diff [1;2;3;1;2;3;4] [1;2;3]) [4]
let my_set_diff_test4 = equal_sets (set_diff [] [1]) []
let my_set_diff_test5 = equal_sets (set_diff [1;5] [1;3]) [5]
let my_set_diff_test6 = equal_sets (set_diff [1] [1;2]) []
let my_set_diff_test7 = equal_sets (set_diff [1;2;1] []) [1;2]

let my_computed_fixed_point_test0 = ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.) (fun x -> x /. 2.) 10.) = 1.25)
let my_computed_fixed_point_test1 = computed_fixed_point (=) sqrt 64. = 1.
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x *. 4.) 5. = infinity
let my_computed_fixed_point_test3 = computed_fixed_point (=) (fun x -> x / 2) 500 = 0

type symbols_1 =
| A | B | C | D | E | F

let rules_1 = [(A, [N B; N C; N D; N E; N F;]); (B, [N C; N D]); (C, [N D]); (D, [T 1]); (E, [N A; N F]); (F, [T 2])]

let my_filter_reachable_test1 = filter_reachable (A, rules_1) = (A, rules_1)
let my_filter_reachable_test1 = filter_reachable (B, rules_1) = (B, [(B, [N C; N D]); (C, [N D]); (D, [T 1])])
let my_filter_reachable_test2 = filter_reachable (C, rules_1) = (C, [(C, [N D]); (D,[T 1])])
let my_filter_reachable_test3 = filter_reachable (D, rules_1) = (D, [(D,[T 1])])
let my_filter_reachable_test4 = filter_reachable (E, rules_1) = (E, rules_1)
let my_filter_reachable_test5 = filter_reachable (F, rules_1) = (F, [(F,[T 2])])















