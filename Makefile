all:
	ghc -dynamic Main

FORCE:

AutoMake-rules.md: FORCE
	pandoc --number-sections --listings rules.md -o rules.pdf

test: FORCE
	./Main eval 7 examples/id.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/inter.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/subset.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/equality.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/welltypedpolymorphism.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/deftest.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/contextmixing.coc | grep "^Right " > /dev/null

	./Main eval 7 examples/add.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/exp.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/exp22.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/zero.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/Nat.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/one.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/two.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/three.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/four.coc | grep "^Right " > /dev/null
	./Main eval 7 examples/five.coc | grep "^Right " > /dev/null

	./Main eval 7 examples/break.coc | grep "^Left " > /dev/null
	./Main eval 7 examples/break2.coc | grep "^Left " > /dev/null

	./Main checktype 7 examples/proofreflexivityofequality.coc examples/propositionreflexivityofequality.coc | grep "==" > /dev/null
	./Main checktype 7 examples/proofsymmetryofequality.coc examples/propositionsymmetryofequality.coc | grep "==" > /dev/null
	./Main checktype 7 examples/prooftransitivityofequality.coc examples/propositiontransitivityofequality.coc | grep "==" > /dev/null
	./Main checktype 7 examples/proofinterissubset.coc examples/propositioninterissubset.coc | grep "==" > /dev/null
