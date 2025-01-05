(* use "03.sml"; *)
val _ = print "\n~~~~~~~~ zip ~~~~~~~~\n";
val test_type: 'a list * 'b list -> ('a * 'b) list = zip;
val PASSED = zip ([], []) = [];
val PASSED = zip ([], [2, 3]) = [];
val PASSED = zip ([1, 2, 3], []) = [];
val PASSED = zip ([1, 2, 3], [4, 5, 6]) = [(1,4), (2,5), (3,6)];
val PASSED = zip ([1, 2, 3], [4, 5]) = [(1,4), (2,5)];
val PASSED = zip ([1, 2], [4, 5, 6]) = [(1,4), (2,5)];
val PASSED = zip ([1, 2], ["a", "b", "c"]) = [(1,"a"), (2,"b")];

val _ = print "\n~~~~~~~~ unzip ~~~~~~~~\n";
val test_type: ('a * 'b) list -> 'a list * 'b list = unzip;
val PASSED = unzip ([]) = ([], []);
val PASSED = unzip ([(2,4), (3,5)]) = ([2, 3], [4, 5]);
val PASSED = unzip ([(1, "a"), (2, "b"), (3, "c")]) = ([1, 2, 3], ["a", "b", "c"]);

val _ = print "\n~~~~~~~~ subtract ~~~~~~~~\n";
val test_type: natural * natural -> natural = subtract;
val PASSED = subtract(Succ (Succ (Succ One)), One) = Succ (Succ One);
val PASSED = subtract(Succ (Succ (Succ One)), Succ (Succ One)) = One;