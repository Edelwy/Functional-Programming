(* Vrne naslednika števila `n`. *)
fun next (n : int) : int = 
    n + 1;

(* Vrne vsoto števil `a` in `b`. *)
fun add (a : int, b : int) : int =
    a + b;

(* Vrne true, če sta vsaj dva argumenta true, drugače vrne false *)
fun majority (a : bool, b : bool, c : bool) : bool =
    (a andalso b) orelse (a andalso c) orelse (b andalso c);

(* Vrne mediano argumentov - števila tipa real brez (inf : real), (~inf : real), (nan : real) in (~0.0 : real)
   namig: uporabi Real.max in Real.min *)
fun median (a : real, b : real, c : real) : real =
    a + b + c - Real.min(a, Real.min(b, c)) - Real.max(a, Real.max(b, c));
    
(* Preveri ali so argumenti veljavne dolžine stranic nekega trikotnika - trikotnik ni izrojen *)
fun triangle (a : int, b : int, c : int)  : bool =
    a + b > c andalso a + c > b andalso b + c > a
    andalso a > 0 andalso b > 0 andalso c > 0;