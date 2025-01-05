(*  Vrne "zippan" seznam iz dveh seznamov. V primeru različnih dolžin,
    krajši seznam določi zadnji par. *)
fun zip ([] : 'a list, y : 'b list ) : ('a * 'b) list = [] 
    | zip (x : 'a list, [] : 'b list) : ('a * 'b) list = [] 
    | zip (x::xs : 'a list, y::ys : 'b list) : ('a * 'b) list = (x, y) :: zip(xs, ys);

(*  Vrne prvi element para. *)
fun fst (x, _) = x;

(*  Vrne drugi element para. *)
fun snd (_, y) = y;

(*  Vrne "uzippana" seznama iz seznama parov. *)
fun unzip ([] : ('a * 'b) list) : ('a list * 'b list) = ([], []) 
    | unzip ((a,b)::x : ('a * 'b) list) : ('a list * 'b list) = (a::(fst (unzip(x))), b::(snd(unzip(x))));

(*  Vrne naravno število, ki ustreza razliki števil a  in b. 
    Sproži izjemo če število ni naravno število.*)
datatype natural = Succ of natural | One;
exception NotNaturalNumber;

fun subtract (One : natural, Succ(b) : natural) : natural = raise NotNaturalNumber
    | subtract (Succ(a) : natural, Succ(b) : natural) : natural = subtract(a, b)
    | subtract (Succ(a), One) = a
    | subtract (One, One) = One;

(*  Vrne true če katerikoli element iz seznama vrne true za funkcijo f.
    Če je seznam prazen vrne false. *)
fun any (f : ('a -> bool), [] : 'a list) : bool = false
    | any (f : ('a -> bool), x::xs : 'a list ) : bool = (f(x) orelse any(f, xs));

(*  Preslika vse elemente seznama s funkcijo f. *)
fun map(f : ('a -> 'b), [] : 'a list) : 'b list = []
    | map(f : ('a -> 'b), x::xs : 'a list) : 'b list = f(x)::map(f, xs);

(*  Vrne true če katerikoli element iz seznama vrne true za funkcijo f.
    Če je seznam prazen vrne false. *)
fun filter (f : ('a -> bool), [] : 'a list) : 'a list = []
    | filter (f : ('a -> bool), x::xs : 'a list ) : 'a list = 
        if f(x) = true then x::filter(f, xs)
        else filter(f,xs);

(* Vrne f(... f(z, x)) tipa 'a. *)
fun fold (f : ('a * 'b -> 'a), z : 'a, [] : 'b list) : 'a = z
    | fold (f : ('a * 'b -> 'a), z : 'a, x::xs : 'b list) : 'a = fold(f, f(z,x), xs);

(* Vrne rotirano drevo v levo (L) ali desno (R) če se to le da. *)
datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun rotateLeft (lf : 'a bstree) : 'a bstree = lf
    | rotateLeft (br(l, x, lf) : 'a bstree) : 'a bstree = br(l, x, lf) 
    | rotateLeft (br(l, x, br(rL, y, rR)) : 'a bstree ) : 'a bstree = br(br(l, x, rL), y, rR);

fun rotateRight lf = lf  
  | rotateRight (br(lf, x, r)) = br(lf, x, r)  
  | rotateRight (br(br(lL, y, lR), x, r)) = br(lL, y, br(lR, x, r));

fun rotate (drevo, L) = rotateLeft(drevo)
    | rotate (drevo, R) = rotateRight(drevo);





       


