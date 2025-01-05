(* =================== ENVIRONMENT SETTINGS. =================== *)

(* settings for long expressions *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;

(* disable polyEq warnings *)
val _ = Control.polyEqWarn := false;

(* datatype for logical formulas *)
datatype 'a expression = 
      Not of 'a expression
    | Or of 'a expression list
    | And of 'a expression list
    | Eq of 'a expression list
    | Imp of 'a expression * 'a expression
    | Var of 'a
    | True 
    | False;

(* linear congurence random number generator.` *)
datatype 'a stream = Next of 'a * (unit -> 'a stream);

fun lcg seed =
    let fun lcg seed =
        Next (seed, fn () =>
            lcg (LargeInt.mod (1103515245 * seed + 12345, 0x7FFFFFFF)))
    in lcg (LargeInt.fromInt seed)
    end;

fun int2bool i = LargeInt.mod (i, 2) = 1;

(* conjutive normal form tester. *)
fun isCNF (And es) =
    List.all
        (fn Or es => List.all (fn (Var _ | Not (Var _)) => true | _ => false) es
        |   (Var _ | Not (Var _)) => true
        |   _ => false) es
|   isCNF (Or es) = List.all (fn (Var _ | Not (Var _)) => true | _ => false) es
|   isCNF (True | False | Var _ | Not (Var _)) = true
|   isCNF _ = false;

(* exception for function `satSolver` *)
exception InvalidCNF;

(* ==================== SOME HELPER FUN. ==================== *)

(* operator for low priority right associative applications *)
infixr 1 $;
fun f $ x = f x;

(* curried equlity test *)
fun eq a b = a = b;

(* curried inequlity test *)
fun neq a b = a <> b;

(* removes all occurrences of `x` from a list *)
fun remove x = List.filter(neq x);

(* returns true if list contains x*)
fun contains x = List.exists(eq x);

(* removes all occurences of elements in list from a list *)
fun diff list = List.filter(fn x => not (contains(x)(list)));

(* exception for nonimplemented functions *)
exception NotImplemented;

(* ==================== WARMUP ==================== *)

fun isolate([]) = []
    | isolate(element::list) = element::isolate(remove(element)(list));

fun negate(expList) = List.map(fn exp => (Not exp))(expList);

(* ==================== PART 1 ==================== *)

fun getVars(Var x) = [x]
    | getVars(Not exp) = getVars(exp)
    | getVars(Or expList) = isolate(List.concat(List.map(getVars)(expList)))
    | getVars(And expList) = isolate(List.concat(List.map(getVars)(expList)))
    | getVars(Eq expList) = isolate(List.concat(List.map(getVars)(expList)))
    | getVars(Imp (exp1, exp2)) = isolate(getVars(exp1)@getVars(exp2))
    | getVars(True) = []
    | getVars(False) = [];

fun eval(list)(True) = true
    | eval(list)(False) = false
    | eval(list)(Var x) = List.exists(eq x)(list)
    | eval(list)(Not exp) = not(eval(list)(exp))
    | eval(list)(Or []) = false
    | eval(list)(Or expList) = List.exists(eval(list))(expList)
    | eval(list)(And []) = true
    | eval(list)(And expList) = List.all(eval(list))(expList)
    | eval(list)(Eq []) = true
    | eval(list)(Eq (exp1::expList)) = List.all (fn exp2 => eval(list)(exp1) = eval(list)(exp2))(expList)
    | eval(list)(Imp (exp1, exp2)) = not(eval(list)(exp1)) orelse eval(list)(exp2);

fun rmEmpty(True) = True
    | rmEmpty(False) = False
    | rmEmpty(Var x) = Var x
    | rmEmpty(Or []) = False
    | rmEmpty(And []) = True
    | rmEmpty(Eq []) = True
    | rmEmpty(And [exp]) = rmEmpty(exp)
    | rmEmpty(Or [exp]) = rmEmpty(exp)
    | rmEmpty(Eq [exp]) = True
    | rmEmpty(Or expList) = Or (List.map(rmEmpty)(expList))
    | rmEmpty(And expList) = And (List.map(rmEmpty)(expList))
    | rmEmpty(Eq expList) = Eq (List.map(rmEmpty)(expList))
    | rmEmpty(Imp (exp1, exp2)) = Imp (rmEmpty(exp1), rmEmpty(exp2))
    | rmEmpty(Not exp) = Not (rmEmpty(exp));

fun pushNegations(exp) = 
    let 
        val removedEmpty = rmEmpty(exp)
    in case removedEmpty of 
        Not (Not exp) => rmEmpty(pushNegations(exp))
        | Not (And expList) => rmEmpty(Or (pushNegationsList(negate(expList))))
        | Not (Or expList) => rmEmpty(And (pushNegationsList(negate(expList))))
        | Not (Imp (exp1, exp2)) => rmEmpty(And [pushNegations(rmEmpty(exp1)), pushNegations(Not (rmEmpty(exp2)))])
        | Not (Eq expList) => rmEmpty(And [Or (pushNegationsList(negate(expList))), Or (pushNegationsList(expList))])
        | True => True
        | False => False
        | Var x => Var x
        | Not exp => rmEmpty(Not (pushNegations(exp)))
        | And expList => rmEmpty(And (pushNegationsList(expList)))
        | Or expList => rmEmpty(Or (pushNegationsList(expList)))
        | Eq expList => rmEmpty(Eq (pushNegationsList(expList)))
        | Imp (exp1, exp2) => rmEmpty(Imp (pushNegations(exp1), pushNegations(exp2)))
    end

and pushNegationsList(expList) = List.map(fn exp => pushNegations(exp))(expList);

fun rmConstants(exp) = 
    let  
        val removedEmpty = rmEmpty(exp)
    in case removedEmpty of 
        True => True
        | False => False
        | Var x => Var x
        | Not exp => 
            let 
                val result = rmEmpty(rmConstants(exp))
            in
                if result = True then False
                else if result = False then True
                else Not result
            end
        | And expList =>
            let 
                val result = rmConstantsList(expList)
            in
                if contains(False)(result) then False 
                else rmEmpty(And (remove(True)(result)))
            end
        | Or expList => 
            let 
                val result = rmConstantsList(expList)
            in
                if contains(True)(result) then True 
                else rmEmpty(Or (remove(False)(result)))
            end
        | Eq expList =>  
            let 
                val result = rmConstantsList(expList)
            in
                if contains(False)(result) andalso contains(True)(result) then False 
                else if contains(False)(result) then rmEmpty(And (negate(remove(False)(result))))
                else if contains(True)(result) then rmEmpty(And (remove(True)(result)))
                else rmEmpty(Eq result)
            end
        | Imp (exp1, exp2) => 
            let 
                val result1 = rmConstants(exp1)
                val result2 = rmConstants(exp2)
            in
                if result1 = False then True
                else if result2 = True then True
                else if result1 = True then rmEmpty(result2)
                else if result2 = False then rmEmpty(Not result1)
                else rmEmpty(Imp (result1, result2))
            end
        end

and rmConstantsList(expList) = List.map(fn exp => rmConstants(exp))(expList);

fun rmVars(exp) = 
    let  
        val removedEmpty = rmEmpty(exp)
    in case removedEmpty of 
        True => True
        | False => False
        | Var x => Var x
        | Not exp => rmEmpty(Not (rmVars(exp)))
        | And expList => rmEmpty(And (isolate(rmVarsList(expList))))
        | Or expList => rmEmpty(Or (isolate(rmVarsList(expList))))
        | Eq expList => rmEmpty(Eq (isolate(rmVarsList(expList))))
        | Imp (exp1, exp2) => 
            if rmVars(exp1) = rmVars(exp2) then True 
            else rmEmpty(Imp (rmVars(exp1), rmVars(exp2)))
    end

and rmVarsList(expList) = List.map(fn exp => rmVars(exp))(expList);

fun simplify(exp) =
    let 
        val simExp = rmVars(pushNegations(rmConstants(exp)))
    in  
        if simExp = exp then simExp
        else simplify(simExp)
    end;

fun prTestEq(seed)(exp1)(exp2) = 
    let 
        val intStream = lcg(seed)
        fun getFirst(Next(x, tailStream)) = x;
        fun getTail(Next(x, tailStream)) = tailStream ();

        fun filterVars(intStream, prevAll, prevChosen, []) = []
            | filterVars(intStream, prevAll, prevChosen, var::vars) = 
                if contains(var)(prevAll) then case List.find(fn x => var = x)(prevChosen) of
                    NONE => filterVars((getTail(intStream), prevAll, prevChosen, vars))
                    | SOME (x) => var::filterVars((getTail(intStream), prevAll, prevChosen, vars))

                else if int2bool(getFirst(intStream)) then var::filterVars((getTail(intStream), prevAll, prevChosen, vars))
                else filterVars((getTail(intStream), prevAll, prevChosen, vars))
        
        val vars1 = filterVars(intStream, [], [], getVars(exp1))
        val vars2 = filterVars(intStream, getVars(exp1), vars1, getVars(exp2))
    in 
        eval(vars1)(exp1) = eval(vars2)(exp2)
    end;

fun getUnitClause(Var x) = SOME(x, True)
  | getUnitClause(Not (Var x)) = SOME(x, False)
  | getUnitClause(Or [Var x]) = SOME(x, True)
  | getUnitClause(Or [Not (Var x)]) = SOME(x, False)
  | getUnitClause(And (exp::expList)) = 
    let 
        val result = getUnitClause(exp)
    in 
        case result of 
            SOME(x, value) => SOME(x, value)
            | NONE => getUnitClause(And expList)
    end
  | getUnitClause(exp) = NONE;

fun updateExpression(var)(value)(Var x) = if x = var then value else Var x
    | updateExpression(var)(value)(Not (Var x)) = if x = var then (if value = True then False else True) else Not (Var x)
    | updateExpression(var)(value)(Or expList) = 
        let 
            val result = List.map(updateExpression(var)(value))(expList)
        in 
            if contains(True)(result) then True
            else if List.length(result) = 0 then False
            else (Or (remove(False)(result)))
        end
    | updateExpression(var)(value)(And expList) = 
        let 
            val result = List.map(updateExpression(var)(value))(expList)
        in 
            if contains(False)(result) then False
            else if List.length(result) = 0 then True
            else (And (remove(True)(result)))
        end
    | updateExpression(var)(value)(exp) = exp;

fun DPLL(True)(vars) = SOME(vars)
    | DPLL(False)(vars) = NONE
    | DPLL(exp)(vars) =
        case getUnitClause(exp) of
            SOME(var, value) => DPLL(updateExpression(var)(value)(exp))(if value = True then var::vars else vars)
            | NONE => 
                if List.length(getVars(exp)) = 0 then DPLL(simplify(exp))(vars)
                else 
                    let 
                        val chosenVar = List.hd(getVars(exp))
                    in case DPLL(updateExpression(chosenVar)(True)(exp))(chosenVar::vars) of
                        SOME(result) => SOME(result)
                        | NONE => DPLL(updateExpression(chosenVar)(False)(exp))(vars)
                    end
                    
fun satSolver(exp) = if isCNF(exp) then DPLL(exp)[] else raise InvalidCNF;

(* ==================== PART 2 ==================== *)

type timetable = {day : string, time: int, course: string} list;

type student = {studentID : int, curriculum : string list};

type XSCTP = {studentID : int, course : string, cycle : string * int, seat : int};

fun attendAllCoursesConstraint(seats)(timetable)(students) = 
    generateVarsPerStudent(seats)(timetable)(students)

and generateVarsPerStudent (seats)(timetable)(students) = 
    List.concat(List.map(fn {studentID = id, curriculum = cur} => generateVarsPerCourse(seats)(timetable)(id)(cur))(students))

and generateVarsPerCourse (seats)(timetable)(studentId)(curriculum) = 
    List.map(fn course => Or (generateVarsPerCycle(seats)(timetable)(studentId)(course)))(curriculum)

and generateVarsPerCycle(seats)(timetable)(studentId)(course) =   
    let
        val filteredTimetable = List.filter(fn {day = d, time = t, course = c} => c = course)(timetable)   
    in         
        List.concat(List.map(fn {day = d, time = t, course = c} => generateVarsPerSeat(seats)(d, t)(studentId)(course))(filteredTimetable))
    end

and generateVarsPerSeat(0)(day, time)(studentId)(course) = []
    | generateVarsPerSeat(seat)(day, time)(studentId)(course) = 
        let 
            val var = Var {studentID = studentId, course = course, cycle = (day, time), seat = seat}
        in
            var::generateVarsPerSeat(seat - 1)(day, time)(studentId)(course)
        end;           

fun noOverlapConstraint(seats)(timetable)(students) =
    generateVarsPerStudent(seats)(timetable)(students)

and generateVarsPerStudent(seats)(timetable)(students) = 
    List.concat(List.map(fn {studentID = id, curriculum = cur} => generateVarsPerCourse(seats)(timetable)(id)(cur))(students))

and generateVarsPerCourse (seats)(timetable)(studentId)(curriculum) = 
    let
        val combinations = List.concat(List.map(fn course1 => List.map(fn course2 => (course1, course2))(curriculum))(curriculum))
    in
        List.concat(List.map(fn (course1, course2) => generateVarsPerCycle(seats)(timetable)(studentId)(course1, course2))(combinations))
    end

and generateVarsPerCycle(seats)(timetable)(studentId)(course1, course2) =   
    let
        val filteredTimetable1 = List.filter(fn {day = d, time = t, course = c} => c = course1)(timetable)   
        val filteredTimetable2 = List.filter(fn {day = d, time = t, course = c} => c = course2)(timetable)   
        val combinations = List.concat(List.map(fn {day = d1, time = t1, course = c1} => 
            List.map(fn {day = d2, time = t2, course = c2} => ((d1, t1, c1),(d2, t2, t2)))(filteredTimetable2))(filteredTimetable1))
    in          
        List.concat(List.map(fn ((d1, t1, c1), (d2, t2, c2)) => generateVarsPerSeat(seats, seats)(d1, t1, d2, t2)(studentId)(course1, course2))(combinations))
    end

and generateVarsPerSeat(0, seat2)(day1, time1, day2, time2)(studentId)(course1, course2) = []
    | generateVarsPerSeat(seat1, 0)(day1, time1, day2, time2)(studentId)(course1, course2) = 
        generateVarsPerSeat(seat1 - 1, seat1)(day1, time1, day2, time2)(studentId)(course1, course2)
    | generateVarsPerSeat(seat1, seat2)(day1, time1, day2, time2)(studentId)(course1, course2) = 
        let 
            val var = Or [Not (Var {studentID = studentId, course = course1, cycle = (day1, time1), seat = seat1}),
                          Not (Var {studentID = studentId, course = course2, cycle = (day2, time2), seat = seat2})]
        in
            if (day1 = day2 andalso time1 = time2) andalso not (course1 = course2 andalso seat1 = seat2) then
            var::generateVarsPerSeat(seat1, seat2 - 1)(day1, time1, day2, time2)(studentId)(course1, course2)
            else generateVarsPerSeat(seat1, seat2 - 1)(day1, time1, day2, time2)(studentId)(course1, course2)
        end; 

fun oneStudentPerSeatConstraint(seats)(timetable)(students) = 
    generateVarsPerCourse(seats)(timetable)(students)

and generateVarsPerCourse(seats)(timetable)(students) = 
   List.concat(List.map(fn {day = d, time = t, course = c} => generateVarsPerStudent(seats)(d, t, c)(students))(timetable))

and generateVarsPerStudent(seats)(day, time, course)(students) = 
    let
        val combinations = List.concat(List.map(fn {studentID = id1, curriculum = cur1} => 
            List.map(fn {studentID = id2, curriculum = cur2} => ((id1, cur1),(id2, cur2)))(students))(students))
    in
        List.concat(List.map(fn ((id1, cur1),(id2, cur2)) => 
            generateVarsPerSeat(seats)(day, time)(id1, cur1, id2, cur2)(course))(combinations))
    end

and generateVarsPerSeat(0)(day, time)(id1, cur1, id2, cur2)(course) = []
    | generateVarsPerSeat(seat)(day, time)(id1, cur1, id2, cur2)(course) = 
        let 
           val var = Or [Not (Var {studentID = id1, course = course, cycle = (day, time), seat = seat}),
                          Not (Var {studentID = id2, course = course, cycle = (day, time), seat = seat})]
        in
            if (contains(course)(cur1) andalso contains(course)(cur2) andalso id1 <> id2) then
            var::generateVarsPerSeat(seat - 1)(day, time)(id1, cur1, id2, cur2)(course)
            else []
        end;
   

fun problemReduction(seats : int)(timetable : timetable)(students : student list) : XSCTP expression = 
    And (List.concat([attendAllCoursesConstraint(seats)(timetable)(students),
        noOverlapConstraint(seats)(timetable)(students),
        oneStudentPerSeatConstraint(seats)(timetable)(students)]));

fun solutionRepresentation(NONE) = NONE
    | solutionRepresentation(SOME(satSolverResult)) = SOME(groupByStudent(satSolverResult))

and groupByStudent(satSolverResult : XSCTP list) : (student * timetable) list =
    let
        val studentIDs = isolate(List.map(fn {studentID = id, course = c, cycle = (d, t), seat = p} => id)(satSolverResult))
        fun getVariables(studentID : int) : XSCTP list = 
            List.filter(fn {studentID = id, course = c, cycle = (d, t), seat = p} => id = studentID)(satSolverResult)   
        fun getTimetable(studentID : int) : timetable = 
            List.map(fn {studentID = id, course = c, cycle = (d, t), seat = p} => {day = d, time = t, course = c})(getVariables(studentID))
        fun getCourses(studentID : int) : string list = isolate(List.map(fn {studentID = id, course = c, cycle = (d, t), seat = p} => c)(getVariables(studentID)))      
    in
        List.map(fn studentID => ({studentID = studentID, curriculum = getCourses(studentID)}, getTimetable(studentID)))(studentIDs)
    end;