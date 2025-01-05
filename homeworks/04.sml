(* Podan seznam xs agregira z začetno vrednostjo z in funkcijo f v vrednost f (f (f z s_1) s_2) s_3) ... *)
fun reduce(f : 'a -> 'b -> 'a)(z : 'a)( [] : 'b list) : 'a = z
    | reduce(f : 'a -> 'b -> 'a)(z : 'a)(x::xs : 'b list) : 'a = reduce(f)(f z x)(xs);

(* Vrne seznam, ki vsebuje kvadrate števil iz vhodnega seznama. Uporabite List.map. *)
fun squares (xs : int list) : int list = List.map (fn x => x * x)(xs);

(* Vrne seznam, ki vsebuje vsa soda števila iz vhodnega seznama. Uporabite List.filter. *)
fun onlyEven (xs : int list) : int list = List.filter(fn x => x mod 2 = 0)(xs);

(* Vrne najboljši niz glede na funkcijo f (prvi arg.). Funkcija f primerja dva niza in vrne true, *)
(* če je prvi niz boljši od drugega. Uporabite List.foldl. Najboljši niz v praznem seznamu je prazen niz. *)
fun bestString (f : string * string -> bool)([] : string list) : string = ""
    | bestString (f : string * string -> bool)(x::xs : string list) : string =
        List.foldl(fn (s1, s2) => if f (s1, s2) then s1 else s2)(x)(xs);

(* Vrne leksikografsko največji niz. Uporabite bestString. *)
fun largestString(xs : string list) : string = bestString( fn (s1, s2) => s1 >= s2 )(xs);
 
(* Vrne najdaljši niz. Uporabite bestString. *)
fun longestString (xs : string list ) : string = bestString( fn (s1, s2) => size(s1) >= size(s2) )(xs);

(* Seznam uredi naraščajoče z algoritmom quicksort. Prvi argument je funkcija za primerjanje. *)
fun quicksort ( f : 'a * 'a -> order)([] : 'a list ) : 'a list = []
    | quicksort ( f : 'a * 'a -> order)(x::xs : 'a list ) : 'a list = 
      let
          val (positive : 'a list, negative : 'a list) = List.partition(fn y => f(x, y) = LESS orelse f(x, y) = EQUAL)(xs)
      in
          quicksort(f)(positive)@[x]@quicksort(f)(negative)
      end;

(* Vrne skalarni produkt dveh vektorjev. Uporabite List.foldl in ListPair.map. *)
fun dot (xs : int list)(xy : int list) : int = 
    List.foldl(fn (x, y) => x + y)(0)(ListPair.map(fn (x, y) => x * y)(xs, xy))

(* Vrne transponirano matriko. Matrika je podana z vrstičnimi vektorji od zgoraj navzdol:
  [[1,2,3],[4,5,6],[7,8,9]] predstavlja matriko
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ] *)
fun transpose (matrix : 'a list list) : 'a list list =
    let 
      val numRows = length(matrix)
      val numCols = if numRows = 0 then 0 else length(hd(matrix))
   in
      if numRows = 0 orelse numCols = 0 then []
      else List.tabulate(numCols, fn j => List.tabulate(numRows, fn i => List.nth(List.nth(matrix, i), j)))
   end;

(* Zmnoži dve matriki. Uporabite dot in transpose. *)
fun multiply ( matrixA : int list list)(matrixB : int list list) : int list list =
    List.map(fn row1 => List.map(fn row2 => dot(row1)(row2))(transpose(matrixB)))(matrixA);

(* V podanem seznamu prešteje zaporedne enake elemente in vrne seznam parov (vrednost, število ponovitev). *)
(* Podobno deluje UNIX-ovo orodje uniq -c. *)
fun group ( [] : ''a list ) : (''a * int) list = []
    | group ( x::xs : ''a list ) : (''a * int) list =
        (x, List.length(List.filter(fn y => x = y)(x::xs)))::group(List.filter(fn y => x <> y)(xs));

(* Elemente iz podanega seznama razvrsti v ekvivalenčne razrede. *)
(* Znotraj razredov naj bodo elementi v istem vrstnem redu kot v podanem seznamu. *)
(* Ekvivalentnost elementov definira funkcija f, ki za dva elementa vrne true, če sta ekvivalentna. *)
fun equivalenceClasses ( f : 'a -> 'a -> bool)([] : 'a list) : 'a list list = []
    | equivalenceClasses ( f : 'a -> 'a -> bool)(x::xs : 'a list) : 'a list list =
        (x::List.filter(fn y => f(x)(y))(xs))::equivalenceClasses(f)(List.filter(fn y => not (f(x)(y)))(xs));
    