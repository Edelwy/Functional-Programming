(* use "04.sml"; *)
val _ = print "\n~~~~~~~~ group ~~~~~~~~\n";
val test_type: ''a list -> (''a * int) list = group;
val PASSED = group ([]) = [];
val PASSED = group ([2, 3, 3, 3, 3]) = [(2,1), (3, 4)];
val PASSED = group ([1, 2, 3]) = [(1,1), (2,1), (3,1)];

val _ = print "\n~~~~~~~~ equivalenceClasses ~~~~~~~~\n";
val test_type: ('a -> 'a -> bool) -> 'a list -> 'a list list = equivalenceClasses;
fun f(x)(y) = (x mod 2 = 0 andalso y mod 2 = 0) orelse (x mod 2 <> 0 andalso y mod 2 <> 0);
val PASSED = equivalenceClasses (f)([]) = [];
val PASSED = equivalenceClasses (f)([2, 3, 3, 4, 5]) = [[2, 4],[3, 3, 5]];