signature RATIONAL =
sig
    (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
    eqtype rational

    (* Definirajte izjemo, ki se kliče pri delu z neveljavnimi ulomki - deljenju z nič. *)
    exception BadRational

    (* Vrne racionalno število, ki je rezultat deljenja dveh podanih celih števil. *)
    val makeRational: int * int -> rational

    (* Vrne nasprotno vrednost podanega števila. *)
    val neg: rational -> rational

    (* Vrne obratno vrednost podanega števila. *)
    val inv: rational -> rational

    (* Funkcija za seštevanje. Rezultat operacije naj sledi postavljenim pravilom. *)
    val add: rational * rational -> rational

    (* Funkcija za množenje. Rezultat operacije naj sledi postavljenim pravilom. *)
    val mul: rational * rational -> rational

    (* Vrne niz, ki ustreza podanemu številu.
        Če je število celo, naj vrne niz oblike "x" oz. "~x".
        Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
    val toString: rational -> string
end

structure Rational :> RATIONAL =
struct
    datatype rational = Frac of int * int | Whole of int

    exception BadRational

    fun gcd (a : int, 0 : int) : int = a 
        | gcd (a : int, b : int) : int = 
            if b < 0 then gcd (~b, a mod ~b)
            else gcd (b, a mod b);

    fun makeRational(a : int, 0 : int) : rational = raise BadRational
        | makeRational(a : int, b : int) : rational = 
            let 
                val gcdValue = gcd(a, b);
            in
                if gcdValue = b orelse gcdValue = ~b then Whole (a div b)
                else if (a < 0 andalso b > 0) orelse (a > 0 andalso b < 0) then Frac (~(a div gcdValue), (b div gcdValue))
                else Frac ((a div gcdValue), (b div gcdValue)) 
            end
    
    fun neg (Whole n : rational) : rational = Whole (~n)
        | neg(Frac (a, b) : rational) : rational = 
            if a > 0 andalso b > 0 then makeRational(~a, b)
            else if a > 0 andalso b < 0 then makeRational(a, ~b)
            else if a < 0 andalso b > 0 then makeRational(~a, b)
            else makeRational(a, ~b)
        
    fun inv (Whole 0 : rational) : rational = raise BadRational
        | inv (Whole n : rational) : rational = makeRational(1, n)
        | inv (Frac (a, b) : rational) : rational = makeRational(b, a)

    fun add (Whole n : rational, Whole m : rational) : rational = Whole (n + m)
        | add (Whole n : rational, Frac (a, b) : rational) =  makeRational(n * b + a, b)
        | add (Frac (a, b): rational, Whole n : rational) =  makeRational(n * b + a, b)
        | add (Frac (a, b): rational, Frac (c, d) : rational) =  makeRational(a *d + c * b, b * d)

    fun mul (Whole n : rational, Whole m : rational) : rational = Whole (n * m)
        | mul (Whole n : rational, Frac (a, b) : rational) =  makeRational(n * a, b)
        | mul (Frac (a, b): rational, Whole n : rational) =  makeRational(n * a, b)
        | mul (Frac (a, b) : rational, Frac (c, d) : rational) = makeRational(a * c, b * d)

    fun toString (Whole n) = Int.toString n 
        | toString (Frac (a, b)) = 
            let val Frac(simA, simB) = makeRational(a, b)
            in Int.toString simA ^ "/" ^ Int.toString simB
            end
end

signature EQ =
sig
    type t
    val eq : t -> t -> bool
end

signature SET =
sig
    (* podatkovni tip za elemente množice *)
    type item

    (* podatkovni tip množico *)
    type set

    (* prazna množica *)
    val empty : set

    (* vrne množico s samo podanim elementom *)
    val singleton : item -> set

    (* unija množic *)
    val union : set -> set -> set

    (* razlika množic (prva - druga) *)
    val difference : set -> set -> set

    (* a je prva množica podmnožica druge *)
    val subset : set -> set -> bool
end

funsig SETFN (Eq : EQ) = SET

functor SetFn (Eq : EQ) : SET = 
struct
    type item = Eq.t

    type set = item list

    val empty : set = []

    fun singleton (x : item) : set = [x]

    fun contains (x : item) (s : set) : bool = List.exists(fn y => Eq.eq x y) s

    fun remove (x : item) = List.filter(fn y => not (Eq.eq x y));

    fun isolate([]) = []
        |isolate(element::list) = element::isolate(remove(element)(list));

    fun union (s1 : set) ([] : set) : set = s1
        | union ([] : set) (s2 : set) : set = s2
        | union (s1 : set) (s2 : set) : set = isolate (s1@s2)
        

    fun difference (s1 : set) (s2 : set) : set =
        List.filter (fn x => not (contains x s2)) s1

    fun subset (s1 : set) (s2 : set) : bool =
        List.all (fn x => contains x s2) s1
end