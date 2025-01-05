(* use "00.sml"; *)
val _ = print "~~~~~~~~ next ~~~~~~~~\n";
val test_type: int -> int = next;
val PASSED = next (~1) = 0;
val PASSED = next (5) = 6;
val PASSED = next (1) = 2;

val _ = print "~~~~~~~~ add ~~~~~~~~\n";
val test_type: int * int -> int = add;
val PASSED = add (~1, 1) = 0;
val PASSED = add (~1, 5) = 4;

val _ = print "~~~~~~~~ majority ~~~~~~~~\n";
val test_type: bool * bool * bool -> bool = majority;
val PASSED = majority (false, false, true) = false;
val PASSED = majority (true, false, true) = true;

val _ = print "~~~~~~~~ median ~~~~~~~~\n";
val test_type: real * real * real -> real = median;
val == = Real.==;
infix ==;
val PASSED = median (1.1, ~1.0, 1.0) == 1.0;
val PASSED = median (1.3, ~2.0, 1.0) == 1.0;
val PASSED = median (1.4, ~1.4, 1.4) == 1.4;

val _ = print "~~~~~~~~ triangle ~~~~~~~~\n";
val test_type: int * int * int -> bool = triangle;
val PASSED = triangle (~1, ~1, ~1) = false;
val PASSED = triangle (1, 1, 1) = true;
val PASSED = triangle (5, 3, 4) = true;