module Prover_Test where

import Data.TPTP
import Data.Text
import Data.Set
import qualified Data.Set as Set
import Data.Map
import qualified Data.Map as Map
import Context
import Prover
import Test.HUnit
import Formulas

import FormulaExamples

set34 = Data.Set.insert f4 (Data.Set.singleton f5)
set34a = Data.Set.insert f6 (Data.Set.singleton f7)

two_args :: Set (FirstOrder Unsorted)
two_args = Data.Set.insert f01 (Data.Set.singleton f02)

three_args :: Set (FirstOrder Unsorted)
three_args = Data.Set.insert f1 (Data.Set.insert f01 (Data.Set.singleton f02))


args1 :: Set (FirstOrder Unsorted)
args1 = Data.Set.insert f01 (Data.Set.singleton f02)

args2 :: Set (FirstOrder Unsorted)
args2 = Data.Set.insert f03 (Data.Set.singleton f01)

-- { ({a, b}, b), ({c, a}, c) }
prepargs0 :: Set (Set (FirstOrder Unsorted), Maybe Literal)
prepargs0 = Data.Set.insert (args1, Just p02)
                            (Data.Set.singleton (args2, Just p03))

-- ({ a }, b)
prepargs1 :: Set (Set (FirstOrder Unsorted), Maybe Literal)
prepargs1 = Data.Set.singleton (Data.Set.singleton f01, Just p02)



uargs0 = Data.Set.insert f04 (Data.Set.singleton f05)
uargs0a = Data.Set.map (\ el -> Aspn el Fresh) uargs0

uargs1 = Data.Set.insert f02 (Data.Set.singleton f01)
uargs1a = Data.Set.map (\ el -> Aspn el Fresh) uargs1

-- { (a -> b) -> a }
context_one_a :: Context
context_one_a = from_assumptions (Data.Set.singleton (Aspn f9 Fresh))
-- { (a -> b) -> a,    d -> b -> a -> b }
context_one_a_one_b :: Context
context_one_a_one_b = Context.insert (Aspn f6a Fresh) context_one_a
-- { (a -> b) -> a,    d -> b -> a -> b,   e -> d -> b -> a -> b }
context_one_a_two_b :: Context
context_one_a_two_b = Context.insert (Aspn f4 Fresh) context_one_a_one_b

-- { (a -> b) -> d -> b -> b }
context_one_a_one_b_mod :: Context
context_one_a_one_b_mod = from_assumptions (Data.Set.singleton (Aspn f6b Fresh))
-- { (a -> b) -> d -> b -> b,   (a -> b) -> e -> d -> b -> b  }
context_one_a_two_b_mod :: Context
context_one_a_two_b_mod = Context.insert (Aspn f4a Fresh) context_one_a_one_b_mod


-- { (b -> a) -> a }
context1_one_a :: Context
context1_one_a = from_assumptions (Data.Set.singleton (Aspn f10 Fresh))
-- { (b -> a) -> a,   d -> b -> a -> b }
context1_one_a_one_b :: Context
context1_one_a_one_b = Context.insert (Aspn f6a Fresh) context1_one_a
-- { (b -> a) -> a,   d -> b -> a -> b,    e -> d -> b -> a -> b }
context1_one_a_two_b :: Context
context1_one_a_two_b = Context.insert (Aspn f4 Fresh) context1_one_a_one_b

-- { (b -> a) -> d -> b -> b,    (b -> a) -> e -> d -> b -> b }
context1_one_a_two_b_res :: Context
context1_one_a_two_b_res =
  Context.from_assumptions (Set.fromList [Aspn (f6c) Fresh, Aspn (f4b) Fresh])


-- { a -> a }
context1_one_a_mod :: Context
context1_one_a_mod = from_assumptions (Data.Set.singleton (Aspn f1a Fresh))
-- { a -> a,   d -> b -> a -> b }
context1_one_a_one_b_mod :: Context
context1_one_a_one_b_mod = Context.insert (Aspn f6a Fresh) context1_one_a_mod
-- { a -> a,   d -> b -> a -> b,    e -> d -> b -> a -> b }
context1_one_a_two_b_mod :: Context
context1_one_a_two_b_mod = Context.insert (Aspn f4 Fresh) context1_one_a_one_b_mod


-- { (b -> a) -> a,   (b -> a) -> e -> d -> b -> b }
context2_one_a_one_b :: Context
context2_one_a_one_b = Context.insert (Aspn f4b Fresh) context1_one_a
-- { (b -> a) -> a,   (b -> a) -> e -> d -> b -> b,    e -> d -> b -> a -> b }
context2_one_a_two_b :: Context
context2_one_a_two_b = Context.insert (Aspn f4 Fresh) context2_one_a_one_b


-- { a -> a,   e -> d -> b -> a -> b }
context2_one_a_one_b_mod :: Context
context2_one_a_one_b_mod = Context.insert (Aspn f4 Fresh) context1_one_a_mod


-- { (b -> a) -> e -> d -> b -> b }
context3 :: Context
context3 = from_assumptions (Data.Set.singleton (Aspn f4b Fresh))
-- { (b -> a) -> e -> d -> b -> b,    e -> d -> b -> a -> b }
context3_two_b :: Context
context3_two_b = Context.insert (Aspn f4 Fresh) context3


-- { e -> d -> b -> a -> b }
context3_mod :: Context
context3_mod = from_assumptions (Data.Set.singleton (Aspn f4bb Fresh))



test_iterate_reach  = TestCase $ do
  assertEqual
    "No change since no change marked" 
    (Set.fromList [f01, f1])
    (iterate_reach (Set.fromList [f1]) (Set.fromList [f01, f1], False))
  assertEqual
    "Added one atom resulting from rule" 
    (Set.fromList [f01, f02])
    (iterate_reach (Set.fromList [f1]) (Set.fromList [f01], True))
  assertEqual
    "Added two atoms resulting from two rules (1)" 
    (Set.fromList [f01, f03, f02, f04])
    (iterate_reach (Set.fromList [f1, f8d]) (Set.fromList [f01, f03], True))
  assertEqual
    "Added two atoms resulting from two rules (2)" 
    (Set.fromList [f01, f03, f02, f04])
    (iterate_reach (Set.fromList [f12, f8d]) (Set.fromList [f02, f03], True))


test_simplify2 = TestCase $ do
  assertEqual
    "No change since there are no 0 order axioms" 
    (context_one_a_two_b)
    (simplify2 context_one_a_two_b)
  assertEqual
    "No change since there are no 0 order axioms"
    (context1_one_a_two_b)
    (simplify2 context1_one_a_two_b)
  assertEqual
    "No change since there are no 0 order axioms"
    (from_assumptions (Set.fromList [Aspn f12 Fresh,
                                      Aspn f8d Fresh,
                                      Aspn f01 Fresh,
                                      Aspn f02 Fresh,
                                      Aspn f03 Fresh,
                                      Aspn f04 Fresh]))
    (simplify2 (from_assumptions (Set.fromList [Aspn f12 Fresh,
                                                Aspn f8d Fresh,
                                                Aspn f02 Fresh,
                                                Aspn f03 Fresh])))

  
test_combine_formulas_from_prepargs = TestCase $ do
  assertEqual
    "We expect two formulas in a set" 
    set34
    (combine_formulas_from_prepargs uargs0 prepargs0)
  assertEqual
    "We expect two formulas in a set" 
    set34a
    (combine_formulas_from_prepargs uargs1 prepargs0)
  assertEqual
    "We expect two formulas in a set" 
    (Data.Set.singleton f1)
    (combine_formulas_from_prepargs Data.Set.empty prepargs1)

    

test_transform_preparg = TestCase $ do
  assertEqual
    "We expect atom when the atomic formula is augmented" 
    (Aspn (f02) Fresh)
    (transform_preparg p03 (Aspn (f02) Fresh) prepargs0)
  assertEqual
    "We expect original formula when it does not have target" 
    (Aspn (Connected f03 Conjunction f02) Fresh)
    (transform_preparg p03 (Aspn (Connected f03 Conjunction f02) Fresh) prepargs0)
  assertEqual
    "For differing targets formula is the same" 
    (Aspn (Connected f03 Implication f03) Fresh)
    (transform_preparg p02 (Aspn (Connected f03 Implication f03) Fresh) prepargs0)
  assertEqual
    "No arguments in arguments of the second argument" 
    (Aspn (Connected f8 Implication (Connected f6 Implication f03)) Fresh)
    (transform_preparg p02 (Aspn (Connected f02 Implication f03) Fresh) prepargs0)
  assertEqual
    "No arguments in arguments of the second argument" 
    (Aspn (Connected f8a Implication (Connected f6a Implication f03)) Fresh)
    (transform_preparg p02 (Aspn (Connected (Connected f04 Implication f02) Implication f03) Fresh) prepargs0)
  assertEqual
    "Three elements, one changed" 
    (Aspn (combine_args_target (Set.fromList [f04, f02, f1]) p02) Fresh)
    (transform_preparg p01 (Aspn f6a Fresh) prepargs1)


test_transform_with_unique_axiom_target = TestCase $ do
  assertEqual
    "Expected the same context since there is no such target" 
    (context_one_a_two_b)
    (transform_with_unique_axiom_target context_one_a_two_b p03)
  assertEqual
    "One flat addition" 
    (to_assumptions context_one_a_two_b_mod)
    (to_assumptions (transform_with_unique_axiom_target context_one_a_two_b p01))
  assertEqual
    "Two replaced subformulas with target 'a'"
    (to_assumptions context1_one_a_two_b_res)
    (to_assumptions (transform_with_unique_axiom_target context1_one_a_two_b p01))






test_simplify6 = TestCase $ do
  assertEqual
    "No change since there are leaflets" 
    (context_one_a_two_b)
    (simplify6 context_one_a_two_b p04)
  assertEqual
    "Two replaced subformulas with target 'a'"
    (context1_one_a_two_b_res)
    (simplify6 context1_one_a_two_b p04)


test_myequal = TestCase $ do
  assertEqual
    "mequal"
    (Set.fromList [f1])
    (Set.fromList [f8d])





main = runTestTT $ TestList [
--  test_myequal,
                             test_transform_preparg,
                             test_combine_formulas_from_prepargs,
                             test_transform_with_unique_axiom_target,
                             test_iterate_reach,
                             test_simplify2,
                             test_simplify6
                            ]
