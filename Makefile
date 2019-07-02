# all:
# 	ghc -dynamic Main

FORCE:

AutoMake-rules.md: FORCE
	pandoc --number-sections --listings rules.md -o rules.pdf

test: FORCE
	./Main test 7 id examples/tests.coc
	./Main test 7 deftest examples/tests.coc
	./Main test 7 welltypedpolymorphism examples/tests.coc
	./Main test 7 contextmixing examples/tests.coc

	./Main test 7 inter examples/set.coc
	./Main test 7 subset examples/set.coc
	./Main test 7 interissubset examples/set.coc
	./Main test 7 proofinterissubset examples/set.coc

	./Main test 7 equalitytests examples/equality.coc

	./Main test 7 succ examples/Nat.coc
	./Main test 7 one examples/Nat.coc
	./Main test 7 add examples/Nat.coc
	./Main test 7 exp examples/Nat.coc
	./Main test 7 zero examples/Nat.coc
	./Main test 7 Nat examples/Nat.coc
	./Main test 7 two examples/Nat.coc
	./Main test 7 three examples/Nat.coc
	./Main test 7 four examples/Nat.coc
	./Main test 7 five examples/Nat.coc
	./Main test 7 pred examples/Nat.coc
	./Main test 7 tests examples/Nat.coc

	./Main test 7 main examples/oneequalspredtwo.coc

	./Main test 7 pair examples/pair.coc
	./Main test 7 head examples/pair.coc
	./Main test 7 tail examples/pair.coc

	./Main testfail 7 break examples/break.coc
	./Main testfail 7 break2 examples/break.coc

