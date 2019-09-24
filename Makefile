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
	stack run test 7 extensionalityaxiom examples/equality.coc
	stack run test 7 typeextensionalityaxiom examples/equality.coc

	stack run test 7 succ examples/Nat.coc
	stack run test 7 one examples/Nat.coc
	stack run test 7 add examples/Nat.coc
	stack run test 7 mult examples/Nat.coc
	stack run test 7 exp examples/Nat.coc
	stack run test 7 zero examples/Nat.coc
	stack run test 7 Nat examples/Nat.coc
	stack run test 7 two examples/Nat.coc
	stack run test 7 three examples/Nat.coc
	stack run test 7 four examples/Nat.coc
	stack run test 7 five examples/Nat.coc
	stack run test 7 pred examples/Nat.coc
	stack run test 7 tests examples/Nat.coc
	stack run test 7 iszero examples/Nat.coc
	stack run test 7 ifzero examples/Nat.coc
	stack run test 7 commutativityaddsucc examples/Nat.coc
	stack run test 7 addsuccab examples/Nat.coc
	stack run test 7 succaddab examples/Nat.coc
	stack run test 7 inductionaxiom examples/Nat.coc
	stack run test 7 addLeftNeutral examples/Nat.coc
	stack run test 7 addRightNeutral examples/Nat.coc
	stack run test 7 addSuccLeftSucc examples/Nat.coc
	stack run test 7 addSuccRightSucc examples/Nat.coc
	stack run test 7 addCommutative examples/Nat.coc
	stack run test 7 xismultonex examples/Nat.coc
	stack run test 7 oneisexpxzero examples/Nat.coc

	stack run test 7 main examples/oneequalspredtwo.coc

	stack run test 7 proofsketchtypecheck examples/predsuccequalsid.coc

	stack run test 7 pair examples/pair.coc
	stack run test 7 head examples/pair.coc
	stack run test 7 tail examples/pair.coc
	stack run test 7 headpairxyeqx examples/pair.coc
	stack run test 7 tailpairxyeqy examples/pair.coc
	stack run test 7 headpair examples/pair.coc

	stack run test 7 Bool examples/Bool.coc
	stack run test 7 true examples/Bool.coc
	stack run test 7 false examples/Bool.coc

	stack run test 7 Maybe examples/Maybe.coc
	stack run test 7 just examples/Maybe.coc
	stack run test 7 nothing examples/Maybe.coc
	stack run test 7 maybecase examples/Maybe.coc

	stack run test 7 Either examples/Either.coc
	stack run test 7 left examples/Either.coc
	stack run test 7 right examples/Either.coc
	stack run test 7 cases examples/Either.coc

	stack run test 7 negAorBimpAarrowB examples/lem.coc
	stack run test 7 AarrowBimpnegAorB examples/lem.coc
	stack run test 7 negequivnegalt examples/lem.coc
	stack run test 7 PeirceimpLEM examples/lem.coc
	stack run test 7 LEMimpPeirce examples/lem.coc

	stack run testfail 7 break examples/break.coc
	stack run testfail 7 break2 examples/break.coc

