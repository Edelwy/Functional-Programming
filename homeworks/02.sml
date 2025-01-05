datatype number = Zero | Succ of number | Pred of number;

fun simp (n : number) : number =
    let fun f (Zero, s : number) = s
        | f (Succ n, Pred s) = f (n, s)
        | f (Succ n, s : number) = f (n, Succ s)
        | f (Pred n, Succ s) = f (n, s)
        | f (Pred n, s : number) = f (n, Pred s)
    in 
        f (n, Zero)
    end

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (Zero) : number = Zero
    | neg (Succ n) : number = Pred (neg n)
    | neg (Pred n) : number = Succ (neg n)

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (a : number, Zero) : number = a
    | add (a : number, Succ n) : number = Succ (add (a, n))
    | add (a : number, Pred n) : number = Pred (add (a, n))

(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
(* datatype order = LESS | EQUAL | GREATER *)
fun comp (a : number, b : number) : order =
    let fun f (Zero, Zero) = EQUAL
        | f (Zero, Succ b) = LESS
        | f (Succ a, Zero) = GREATER
        | f (Succ a, Succ b) = f (a, b)
        | f (Zero, Pred b) = GREATER
        | f (Pred a, Zero) = LESS
        | f (Pred a, Pred b) = f (a, b)
        | f (Succ a, Pred b) = GREATER
        | f (Pred a, Succ b) = LESS
    in
        f (simp a, simp b)
    end

datatype tree = Node of int * tree * tree | Leaf of int;

(* Vrne true, če drevo vsebuje element x. *)
fun contains (Leaf v, x : int) : bool = (v = x)
    | contains (Node (v, t1, t2), x : int) : bool = 
    (v = x) orelse contains (t1, x) orelse contains (t2, x)


(* Vrne število listov v drevesu. *)
fun countLeaves (Leaf _) : int = 1
    | countLeaves (Node (_, t1, t2)) : int = 1 + countLeaves t1 + countLeaves t2

(* Vrne število število vej v drevesu. *)
fun countBranches (Leaf _) : int = 0
    | countBranches (Node (_, t1, t2)) : int = 2 + countBranches t1 + countBranches t2

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (Leaf _) : int = 1
    | height (Node (_, t1, t2)) : int =
        let val h1 = height t1
            val h2 = height t2
        in
            1 + (if h1 > h2 then h1 else h2)
        end

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (Leaf v) : int list = [v]
    | toList (Node (v, t1, t2)) : int list = (toList t1) @ [v] @ (toList t2)

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (Leaf _) : bool = true
    | isBalanced (Node (v, t1, t2)) : bool = 
        let val h1 = height t1
            val h2 = height t2
        in
            (abs (h1 - h2) <= 1) andalso isBalanced t1 andalso isBalanced t2 
        end

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun isBST (Leaf _) : bool = true
    | isBST (Node (v, t1, t2)) : bool = 
        let fun gthan (Leaf v, n : int) = (v > n) 
            | gthan (Node (v, t1, t2), n : int) = (v > n) andalso gthan (t1, n) andalso gthan (t2, n)
            fun lthan (Leaf v, n : int) = (v < n) 
            | lthan (Node (v, t1, t2), n : int) = (v < n) andalso lthan (t1, n) andalso lthan (t2, n)
        in 
            lthan (t1, v) andalso gthan (t2, v) andalso isBST t1 andalso isBST t2
        end

