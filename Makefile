all:
	ghc -dynamic Main

FORCE:

AutoMake-rules.md: FORCE
	pandoc --number-sections --listings rules.md -o rules.pdf

test: FORCE
	./Main test 7 examples/id.coc
	./Main test 7 examples/inter.coc
	./Main test 7 examples/subset.coc
	./Main test 7 examples/equality.coc
	./Main test 7 examples/welltypedpolymorphism.coc
	./Main test 7 examples/deftest.coc
	./Main test 7 examples/contextmixing.coc

	./Main test 7 examples/succ.coc
	./Main test 7 examples/add.coc
	./Main test 7 examples/exp.coc
	./Main test 7 examples/exp22.coc
	./Main test 7 examples/zero.coc
	./Main test 7 examples/Nat.coc
	./Main test 7 examples/one.coc
	./Main test 7 examples/two.coc
	./Main test 7 examples/three.coc
	./Main test 7 examples/four.coc
	./Main test 7 examples/five.coc
	./Main test 7 examples/pred.coc
	./Main test 7 examples/predzero.coc
	./Main test 7 examples/predone.coc
	./Main test 7 examples/predtwo.coc

	./Main test 7 examples/pair.coc
	./Main test 7 examples/head.coc
	./Main test 7 examples/tail.coc

	./Main testfail 7 examples/break.coc
	./Main testfail 7 examples/break2.coc

	./Main test 7 examples/oneequalspredtwo.coc
	./Main test 7 examples/reflexivityofequality.coc
	./Main test 7 examples/symmetryofequality.coc
	./Main test 7 examples/transitivityofequality.coc
	./Main test 7 examples/interissubset.coc
