import Test.HUnit
import Data.Map.Strict
import Text.Megaparsec

import CocExpr
import CocUnify
import CocParser

cocParse input = case parse parseCocSyntax "" input of
    Right a -> fromCocSyntax mempty a

unifyTests = TestList [
    (unify
        (cocParse "(\\a:*.a)")
        (cocParse "(\\b:*.b)")
    ) ~?= (Just (fromList [])),
    (unify
        (cocParse "(\\a:*.?a)")
        (cocParse "(\\b:*.b)")
    ) ~?= (Just (fromList [("?a", CocVariable 0 "b")])),
    (unify
        (cocParse "(\\a:*.?a a)")
        (cocParse "(\\b:*.(\\c:*.c) ?b)")
    ) ~?= (Just (fromList [("?a", CocLambda "c" CocProp (CocVariable 0 "c")),
                           ("?b", CocVariable 0 "a")])),
    (unify
        (cocParse "(\\a:*.?a a)")
        (cocParse "(\\b:*.b ?a)")
    ) ~?= (Just (fromList [("?a", CocVariable 0 "a")])),
    (unify
        (cocParse "(\\a:*.?a (\\c:*.c))")
        (cocParse "(\\b:*.b (\\c:*.?a))")
    ) ~?= (Nothing)
    ]

tests = TestList [unifyTests]

main = runTestTT tests
