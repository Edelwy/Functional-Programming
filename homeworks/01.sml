(*  Vrne fakulteto števila n, n >= 0. *)
fun factorial (0 : int) : int = 1
    | factorial (n : int) : int = n * factorial (n - 1) 

(*  Vrne n-to potenco števila x, n >= 0. *)
fun power (x : int, 1 : int) : int = x
    | power (x : int, 0 : int) : int = 1
    | power (x : int, n : int) : int = x * power (x, n - 1);

(*  Vrne največjega skupnega delitelja pozitivnih števil a in b, a >= b. *)
fun gcd (a : int, 0 : int) : int = a 
    | gcd (a : int, b : int) : int = gcd (b, a mod b);

(*  Vrne dolžino seznama. *)
fun len ([]  : int list) : int = 0
    | len (x::xs : int list) : int = 1 + length (xs);

(*  Vrne SOME zadnji element seznama. Če je seznam prazen vrne NONE. *)
fun last ([] : int list) : int option = NONE
    | last (x::xs : int list) : int option  = 
        if len ( xs ) = 0 then SOME (x)
        else last (xs);

(*  Vrne SOME n-ti element seznama. Prvi element ima indeks 0. Če indeks ni veljaven, vrne NONE. *)
fun nth ([] : int list, n : int) : int option = NONE
    | nth (x::xs : int list, n : int) : int option  = 
        if n < 0 then NONE
        else if n = 0 then SOME (x)
        else nth (xs, n - 1);

(*  Vrne nov seznam, ki je tak kot vhodni, le da je na n-to mesto vrinjen element x. Prvo mesto v seznamu ima indeks 0. Indeks n je veljaven (0 <= n <= length xs). *)
fun insert (xs : int list, 0 : int, x : int) : int list = x::xs
    | insert (y::ys : int list, n : int, x : int) : int list = y::insert (ys, n - 1, x);

(*  Vrne nov seznam, ki je tak kot vhodni, le da so vse pojavitve elementa x odstranjene. *)
fun delete ([] : int list, x : int) : int list = [] 
    | delete (y::ys : int list, x : int) : int list =
        if y = x then delete ( ys, x) else y::delete (ys, x);

(*  Doda element x na konec seznama. *)
fun append ([] : int list, x : int ) : int list = [x]
    | append (y::ys : int list, x : int ) : int list = y::append (ys, x);

(*  Vrne obrnjen seznam. *)
fun reverse ( [] : int list) : int list = []
    | reverse( x::xs : int list) : int list = append (reverse ( xs ), x);

(*  Vrne true, če je podani seznam palindrom. Tudi prazen seznam je palindrom. *)
fun palindrome (xs : int list) : bool = xs = reverse( xs)
