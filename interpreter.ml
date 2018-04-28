include Stack ;;
include Hashtbl ;;

(* DATA TYPES ARE DEFINED *****************)

(* Operators Defined*)
type operator = Abs| Not | Plus | Minus | Multiply | Divide | Mod | Exponent | And | Or | Implies | Equal | Great | Less | GreatEq | LessEq | Proj ;;

(* Expression data type defined *)
type exp = 
	| ExpInt of int
	| ExpBool of bool
	| ExpIdent of string
	| ExpUnr of operator * exp
	| ExpBin of exp * operator * exp 
	| NTuple of exp list 
	| ExpProj of operator * int * exp ;;

(* Opcode data type defined *)
type opcode = ConstInt of int | ConstBool of bool | ConstIdent of string | ListOf of exp list |
			  ABS | NOT |
			  PLUS | MINUS | MULTIPLY | DIVIDE | MOD | EXPONENT | AND | OR | IMPLIES | EQUAL | GREAT | LESS | GREATEQ | LESSEQ | PROJ;;

(* Answer data type defined *)
type answer = AnsInt of int | AnsBool of bool | AnsStr of string | AnsProjList of exp list ;;



(* FUNCTIONS ARE DEFINED ********************)

(* Gives the Opcode of Operator *)
let compile_op = function
	| Abs -> ABS
	| Not -> NOT
	| Plus -> PLUS
	| Minus -> MINUS
	| Multiply -> MULTIPLY
	| Divide -> DIVIDE
	| Mod -> MOD
	| Exponent -> EXPONENT
	| And -> AND
	| Or -> OR
	| Implies -> IMPLIES
	| Equal -> EQUAL
	| Great -> GREAT
	| Less -> LESS
	| GreatEq -> GREATEQ
	| LessEq -> LESSEQ 
	| Proj -> PROJ ;;

(* Checks if the Binary operator is Comparison or Computation *)
let type_of_bin op = match op with
	| EXPONENT -> -1
	| PLUS -> 0
	| MINUS -> 0
	| MULTIPLY -> 0
	| DIVIDE -> 0
	| MOD -> 0
	| EXPONENT -> 0 
	| EQUAL -> 1
	| GREAT -> 1
	| LESS -> 1
	| GREATEQ -> 1
	| LESSEQ -> 1 
	| PROJ -> 2 ;;


(* Operations performed by Integer Binary Operators *)
let eval_bin_op_int1 = function
	| PLUS -> (+) 
	| MINUS -> (-) 
	| MULTIPLY -> ( * )
	| DIVIDE -> (/)
	| MOD -> (mod) ;;

let eval_bin_op_int2 = function	
	| EQUAL -> (==)
	| GREAT -> (>)
	| LESS -> (<)
	| GREATEQ -> (>=)
	| LESSEQ -> (<=) ;;


(* Operations performed by Boolean Binary Operators *)
let eval_bin_op_bool = function
	| AND -> (&)
	| OR -> (||);; 	

(* Parses an expression, returns the opcode list *)
let rec compile exp = match exp with
	| ExpInt(n) -> [ConstInt(n)]
	| ExpBool(b) -> [ConstBool(b)]
	| ExpIdent(id) -> [ConstIdent(id)]
	| ExpUnr(un,e) -> (compile e) @ [compile_op un]
	| ExpBin(e1,bi,e2) -> (match bi with 
							Implies -> (compile (ExpUnr(Not, e1))) @ (compile e2) @ [OR]							
							| _ -> (compile e1) @ (compile e2) @ [compile_op bi] )
	| NTuple(e) -> [ListOf(e)]
	| ExpProj(prj,i,l) -> [ConstInt(i)] @ (compile l) @ [compile_op prj] ;;


(* Helper Function: Returns ith element of a list *)
let rec ithElement l i = match (i,l) with 
							  (1, x::xs) -> x 
							| (n,x::xs) -> ithElement xs (n-1) ;;

(* Given a stack and a table, evaluates the expression denoted by the opcode list  *)
let rec execute (s, t, c) = match (s,c) with
	| (s,[]) -> Stack.pop s
	| (s, ConstInt(n)::cs) -> let temp = (Stack.push (AnsInt(n)) s) in execute (s, t, cs)
	| (s, ConstBool(b)::cs) -> let temp = (Stack.push (AnsBool(b)) s) in execute (s, t, cs)
	| (s, ConstIdent(id)::cs) -> let temp = 
									(if (mem t id) then 
										(match (find t id) with
													   (x::y::xs) -> if (y==0) then (Stack.push (AnsInt(x)) s)
													 				 else (Stack.push (AnsBool(x==1)) s)) 
									else (Stack.push (AnsStr(id)) s))
								 in execute (s, t, cs)
	| (s, ListOf(l)::cs) -> let temp = (Stack.push (AnsProjList(l)) s) in execute (s, t, cs)
	| (s, op::cs) -> match Stack.pop s with 
						  AnsInt(n) ->  (match op with 
											  ABS -> let temp = (Stack.push (AnsInt(abs(n))) s) in execute(s, t, cs)
											| bin_op -> (match (type_of_bin bin_op) with 
															  -1 -> (match Stack.pop s with AnsInt(n2) -> let temp = (Stack.push (AnsInt(int_of_float ((float n2)**(float n)))) s) in execute(s, t, cs))
															| 0 -> (match Stack.pop s with AnsInt(n2) -> let temp = (Stack.push (AnsInt(eval_bin_op_int1 bin_op n2 n)) s) in execute(s, t, cs))
															| 1 -> (match Stack.pop s with AnsInt(n2) -> let temp = (Stack.push (AnsBool(eval_bin_op_int2 bin_op n2 n)) s) in execute(s, t, cs))))
						| AnsBool(b) -> (match op with
											  NOT -> let temp = (Stack.push (AnsBool(not(b))) s) in execute(s, t, cs)
											| bin_op -> (match Stack.pop s with AnsBool(b2) -> let temp = (Stack.push (AnsBool(eval_bin_op_bool bin_op b2 b)) s) in execute(s, t, cs))) 
						| AnsProjList(l) -> let v = Stack.pop s in match v with AnsInt(i) -> execute(s, t, (compile (ithElement l i)) @ cs) ;;


(* Symbol/ Look-up Table is defined *)
let size = 3 ;;
let t = Hashtbl.create size ;;
(* Variables with their values are added to the table.*)
(* First element of the Value list is the value and Second element depicts if the variable is an Integer or a Boolean *)
add t "x" [5; 0] ;;  
add t "y" [3; 0] ;;
add t "z" [1; 1] ;;

(* Evaluates the given expression *)
let eval e = execute ((Stack.create()),t,(compile e));;

	

(* Examples ************************)
let exp1 = ExpInt(-3) ;;
let exp2 = ExpBool(true) ;;
let exp3 = ExpIdent("x") ;;
let exp4 = ExpUnr(Not, (ExpIdent("z"))) ;;
let exp5 = ExpBin(exp2, Or, exp4) ;;
let exp6 = ExpBin(exp2, And, exp4) ;;
let exp7 = ExpUnr(Abs, exp1) ;;
let exp8 = ExpBin(exp1, Plus, exp3) ;;
let exp9 = ExpBin(exp3, Minus, exp8) ;;
let exp10 = ExpBin(exp7, Equal, ExpUnr(Abs,exp1)) ;;
let exp11 = ExpBin(exp1, GreatEq, exp3) ;;
let exp12 = ExpProj(Proj, 3 , NTuple([exp7;exp9;exp11])) ;;

let res1 = eval exp1 ;;
let res2 = eval exp2 ;;
let res3 = eval exp3 ;;
let res4 = eval exp4 ;;
let res5 = eval exp5 ;;
let res6 = eval exp6 ;;
let res7 = eval exp7 ;;
let res8 = eval exp8 ;;
let res9 = eval exp9 ;;
let res10 = eval exp10 ;;
let res11 = eval exp11 ;;
let res12 = eval exp12 ;;



