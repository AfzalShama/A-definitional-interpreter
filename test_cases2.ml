# use "assn2.ml";;

eval (ExpBin(ExpInt(1), Plus, ExpInt(2)));;
eval (ExpBin(ExpInt(6), Multiply, ExpInt(6)));;
eval (ExpBin(ExpInt(2), Exponent, ExpInt(4)));;
eval (ExpBin(ExpInt(6), Divide, ExpInt(3)));;
eval (ExpIdent("iden1"));;
eval (ExpIdent("iden2"));;

eval (ExpUnr(Abs, ExpInt(-1)));;
eval (ExpProj(Proj, 2, NTuple([ExpInt(12); ExpInt(121); ExpInt(33)])));;

eval (ExpBin(ExpProj(Proj, 2, NTuple[ExpInt(2); ExpInt(5); ExpInt(8)]), Minus ,ExpInt(1)));;
eval (ExpBin(ExpProj(Proj, 2, NTuple[ExpInt(2); ExpInt(5); ExpInt(8)]), Mod, ExpInt(2)));;

eval (ExpBin(
	ExpBin(ExpInt(5), Equal, ExpInt(5)),
	Or,
	ExpBin(ExpBin(ExpBin(ExpInt(2), Minus, ExpInt(1)), Equal, ExpInt(1)), Implies,
		ExpBin(ExpBin(ExpProj(Proj, 2, NTuple[ExpInt(2); ExpInt(5); ExpInt(8)]), Mod, ExpInt(2)), Equal, ExpInt(1))
	)
));;

eval (ExpBin(ExpBool(true), And, ExpBool(false)));;
eval (ExpBin(ExpUnr(Not, ExpBin(ExpBin(ExpBool(true), Or, ExpBool(false)), Implies, ExpBin(ExpBool(true), And, ExpBool(false)))), Implies, ExpBin(ExpBin(ExpBool(true), And, ExpBool(false)), Implies, ExpBin(ExpBool(true), Or, ExpBool(false)))));;

eval (ExpBin(ExpInt(4), GreatEq, ExpInt(2)));;
eval (ExpBin(ExpInt(4), LessEq, ExpInt(2)));;