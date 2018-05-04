type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

fun lookup ([],m) = ~1 
  | lookup (((n:id,v)::xs),m:id) = 
  if n = m then v
  else (lookup (xs,m))

type table = (id * int) list

fun update (t, n, v) = 
  (n,v) :: t


(* stm * env -> env *)
fun interpStm ((CompoundStm (s1,s2)), t) = 
    let val t' = (interpStm (s1,t))
    in interpStm (s2,t')
    end
  | interpStm ((AssignStm (n,e)), t) =
    let val (v,t') = (interpExp (e,t))
    in update (t', n, v)
    end
  | interpStm ((PrintStm (e::es)), t) = 
    let val (v, t') = (interpExp (e, t))
    in print ((Int.toString v) ^ "\n");
       interpStm ((PrintStm es), t')
    end
  | interpStm ((PrintStm ([])), t) = t
(* exp * table -> int * table *)
and interpExp ((IdExp n), t) = 
    let val v = (lookup (t, n))
    in (v, t)
    end
  | interpExp ((NumExp v), t) = (v,t)
  | interpExp ((OpExp (e1,bop,e2)), t) = 
    let val (v1,t') = (interpExp (e1,t))
        val (v2,t'') = (interpExp (e2,t))
    in case bop of
            Plus => ((v1 + v2), t'')
          | Minus => ((v1 - v2), t'')
          | Times => ((v1 * v2), t'')
          | Div => ((v1 div v2), t'')
    end
  | interpExp ((EseqExp (s,e)), t) = 
    let val t' = interpStm (s, t)
    in interpExp (e, t')
    end
    


fun maxargs (CompoundStm (s1,s2)) = Int.max((maxargs s1),(maxargs s2))
  | maxargs (AssignStm (_, e)) = (maxargse e)
  | maxargs (PrintStm el) = Int.max((length el), (List.foldl Int.max 0 (List.map maxargse el)))
and maxargse (IdExp _) = 0
  | maxargse (NumExp _) = 0
  | maxargse (OpExp (e1,_,e2)) = Int.max((maxargse e1),(maxargse e2))
  | maxargse (EseqExp (s,e)) = Int.max((maxargs s),(maxargse e));

maxargs prog;

interpStm (prog, []);
