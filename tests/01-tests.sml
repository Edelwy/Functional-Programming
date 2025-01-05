(* use "01.sml"; *)
val _ = print "\n~~~~~~~~ factorial ~~~~~~~~\n";
val test_type: int -> int = factorial;
val PASSED = factorial (3) = 6;
val PASSED = factorial (4) = 24;
val PASSED = factorial (2) = 2;
val PASSED = factorial (0) = 1;

val _ = print "\n~~~~~~~~ power ~~~~~~~~\n";
val test_type: int * int -> int = power;
val PASSED = power (3, 1) = 3;
val PASSED = power (4, 0) = 1;
val PASSED = power (2, 3) = 8;
val PASSED = power (~3, 3) = ~27;

val _ = print "\n~~~~~~~~ gcd ~~~~~~~~\n";
val test_type: int * int -> int = gcd;
val PASSED = gcd (3, 1) = 1;
val PASSED = gcd (4, 2) = 2;
val PASSED = gcd (30, 25) = 5;
val PASSED = gcd (30, ~25) = ~5;

val _ = print "\n~~~~~~~~ len ~~~~~~~~\n";
val test_type: int list -> int = len;
val PASSED = len ([]) = 0;
val PASSED = len ([4, 1]) = 2;
val PASSED = len ([1, 2, 5]) = 3;

val _ = print "\n~~~~~~~~ last ~~~~~~~~\n";
val test_type: int list -> int option = last;
val PASSED = last ([]) = NONE;
val PASSED = last ([2]) = SOME(2);
val PASSED = last ([4, 1]) = SOME(1);
val PASSED = last ([1, 2, 5]) = SOME(5);

val _ = print "\n~~~~~~~~ nth ~~~~~~~~\n";
val test_type: int list * int -> int option = nth;
val PASSED = nth ([], 2) = NONE;
val PASSED = nth ([1, 2, 3], 4) = NONE;
val PASSED = nth ([1, 2, 3], ~1) = NONE;
val PASSED = nth ([1, 2, 3], 2) = SOME(3);
val PASSED = nth ([1, 2, 3], 1) = SOME(2);
val PASSED = nth ([1, 2, 3], 0) = SOME(1);

val _ = print "\n~~~~~~~~ insert ~~~~~~~~\n";
val test_type: int list * int * int -> int list = insert;
val PASSED = insert ([1, 2, 3], 0, 0) = [0, 1, 2, 3];
val PASSED = insert ([1, 2, 3], 1, 0) = [1, 0, 2, 3];
val PASSED = insert ([1, 2, 3], 2, 0) = [1, 2, 0, 3];
val PASSED = insert ([1, 2, 3], 3, 0) = [1, 2, 3, 0];

val _ = print "\n~~~~~~~~ delete ~~~~~~~~\n";
val test_type: int list * int -> int list = delete;
val PASSED = delete ([1, 2, 3], 0) = [1, 2, 3];
val PASSED = delete ([1, 2, 3], 1) = [2, 3];
val PASSED = delete ([1, 2, 3], 2) = [1, 3];
val PASSED = delete ([1, 2, 3], 3) = [1, 2];

val _ = print "\n~~~~~~~~ reverse ~~~~~~~~\n";
val test_type: int list -> int list = reverse;
val PASSED = reverse ([]) = [];
val PASSED = reverse ([1]) = [1];
val PASSED = reverse ([1, 2, 3]) = [3, 2, 1];
val PASSED = reverse ([1, 2, 3, 2, 8]) = [8, 2, 3, 2, 1];

val _ = print "\n~~~~~~~~ reverse ~~~~~~~~\n";
val test_type: int list -> int list = reverse;
val PASSED = reverse ([]) = [];
val PASSED = reverse ([1]) = [1];
val PASSED = reverse ([1, 2, 3]) = [3, 2, 1];
val PASSED = reverse ([1, 2, 3, 2, 8]) = [8, 2, 3, 2, 1];

val _ = print "\n~~~~~~~~ palindrome ~~~~~~~~\n";
val test_type: int list -> bool = palindrome;
val PASSED = palindrome ([]) = true;
val PASSED = palindrome ([1]) = true;
val PASSED = palindrome ([1, 2, 2, 1]) = true;
val PASSED = palindrome ([1, 2, 1]) = true;
val PASSED = palindrome ([1, 2, 3]) = false;
val PASSED = palindrome ([1, 2, 3, 4, 5]) = false;
val PASSED = palindrome ([1, 2, 3, 2, 1]) = true;
