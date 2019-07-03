# all:
# 	ghc -dynamic Main

FORCE:

AutoMake-rules.md: FORCE
	pandoc --number-sections --listings rules.md -o rules.pdf

test: FORCE
	stack run test 7 id examples/tests.coc
	stack run test 7 deftest examples/tests.coc
	stack run test 7 welltypedpolymorphism examples/tests.coc
	stack run test 7 contextmixing examples/tests.coc

	stack run test 7 inter examples/set.coc
	stack run test 7 subset examples/set.coc
	stack run test 7 interissubset examples/set.coc
	stack run test 7 proofinterissubset examples/set.coc

	stack run test 7 equalitytests examples/equality.coc

	stack run test 7 succ examples/Nat.coc
	stack run test 7 one examples/Nat.coc
	stack run test 7 add examples/Nat.coc
	stack run test 7 exp examples/Nat.coc
	stack run test 7 zero examples/Nat.coc
	stack run test 7 Nat examples/Nat.coc
	stack run test 7 two examples/Nat.coc
	stack run test 7 three examples/Nat.coc
	stack run test 7 four examples/Nat.coc
	stack run test 7 five examples/Nat.coc
	stack run test 7 pred examples/Nat.coc
	stack run test 7 tests examples/Nat.coc

	stack run test 7 main examples/oneequalspredtwo.coc

	stack run test 7 proofsketchtypecheck examples/predsuccequalsid.coc

	stack run test 7 pair examples/pair.coc
	stack run test 7 head examples/pair.coc
	stack run test 7 tail examples/pair.coc

	stack run testfail 7 break examples/break.coc
	stack run testfail 7 break2 examples/break.coc

