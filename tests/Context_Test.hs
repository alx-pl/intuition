module Main where

import Data.TPTP
import Data.Text
import Data.Set
import qualified Data.Set as Set
import Data.Map
import qualified Data.Map as Map
import Context
import Test.HUnit

import FormulaExamples

set34 = Data.Set.insert f4 (Data.Set.singleton f5)
set34aspn = Data.Set.map (\ el -> Aspn el Fresh) set34
set34a = Data.Set.insert f6 (Data.Set.singleton f7)

two_args :: Set (FirstOrder Unsorted)
two_args = Data.Set.insert f01 (Data.Set.singleton f02)

three_args :: Set (FirstOrder Unsorted)
three_args = Data.Set.insert f1 (Data.Set.insert f01 (Data.Set.singleton f02))

args1 = Data.Set.insert f01 (Data.Set.singleton f02)
args2 = Data.Set.insert f03 (Data.Set.singleton f01)
  
prepargs0 = Data.Set.insert (args1, Just p02)
                            (Data.Set.singleton (args2, Just p03))

uargs0 = Data.Set.insert f04 (Data.Set.singleton f05)
uargs0a = Data.Set.map (\ el -> Aspn el Fresh) uargs0

uargs1 = Data.Set.insert f02 (Data.Set.singleton f01)
uargs1a = Data.Set.map (\ el -> Aspn el Fresh) uargs1

uargs0_map_targets = compute_map_for_axiom_targets uargs0a
-- uargs0_map_leaflets = compute_map_for_leaflets uargs0a

uargs1_map_targets = compute_map_for_axiom_targets uargs1a
-- uargs1_map_leaflets = compute_map_for_leaflets uargs1a

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


--test_from_assumptions = TestCase $ do
--  assertEqual
--    "" 
--    (Ctx uargs0a uargs0_map_targets uargs0_map_leaflets)
--    (from_assumptions uargs0a)
--  assertEqual
--    "" 
--    (Ctx uargs1a uargs1_map_targets uargs1_map_leaflets)
--    (from_assumptions uargs1a)


test_get_formula_for_target = TestCase $ do
--  assertEqual
--    "Expected Nothing" 
--    (Nothing)
--    (get_formula_for_target (Ctx uargs0a uargs0_map_targets uargs0_map_leaflets) p01)
--  assertEqual
--    "Expected formula f05" 
--    (Just f05)
--    (get_formula_for_target (Ctx uargs0a uargs0_map_targets uargs0_map_leaflets) p05)
  assertEqual
    "Expected formula f9" 
    (Just f9)
    (get_formula_for_target context_one_a_two_b p01)


test_get_literals_for_occurrences_atargets = TestCase $ do
  assertEqual
    "Expected singleton with atom 'a'" 
    (Data.Set.singleton p01)
    (get_literals_for_occurrences_atargets context1_one_a_two_b 1)
  assertEqual
    "Expected singleton with atom 'b'" 
    (Data.Set.singleton p02)
    (get_literals_for_occurrences_atargets context1_one_a_two_b 2)


--test_get_literals_for_occurrences_leaflets = TestCase $ do
--  assertEqual
--    "Expected singleton with atom 'b'" 
--    (Data.Set.singleton p02)
--    (get_literals_for_occurrences_leaflets context1_one_a_two_b 1)
--  assertEqual
--    "Expected empty set" 
--    Data.Set.empty
--    (get_literals_for_occurrences_leaflets context1_one_a_two_b 2)


--test_negative_only_in_leaflets = TestCase $ do
--  assertEqual
--    "Expected empty set of literals" 
--    (Data.Set.empty)
--    (negative_only_in_leaflets context_one_a_two_b)
--  assertEqual
--    "Expected singleton with atom 'a'" 
--    (Data.Set.singleton p01)
--    (negative_only_in_leaflets context_one_a_two_b_mod)
--  assertEqual
--    "Expected singleton with atom 'b'" 
--    (Data.Set.singleton p02)
--    (negative_only_in_leaflets context1_one_a)


test_remove_leaflets_in_requirements_for_atom_of_context  = TestCase $ do
  assertEqual
    "Expected single identity context" 
    (from_assumptions (Data.Set.singleton (Aspn f1a Fresh)))
    (remove_leaflets_in_requirements_for_atom_of_context context1_one_a p01)
  assertEqual
    "Expected context with single identity type inside" 
    (context1_one_a_two_b_mod)
    (remove_leaflets_in_requirements_for_atom_of_context context1_one_a_two_b p01)
  assertEqual
    "Expected context with single formula (a)" 
    (context3_mod)
    (remove_leaflets_in_requirements_for_atom_of_context context3 p01)
  assertEqual
    "Expected context with single formula (b)" 
    (context3_mod)
    (remove_leaflets_in_requirements_for_atom_of_context context3 p01)
  assertEqual
    "Expected context with one formula" 
    (context3_mod)
    (remove_leaflets_in_requirements_for_atom_of_context context3_two_b p01)
  assertEqual
    "Expected context with identity type inside and one modified" 
    (context2_one_a_one_b_mod)
    (remove_leaflets_in_requirements_for_atom_of_context context2_one_a_two_b p01)



test_remove_leaflets_in_requirements_of = TestCase $ do
  assertEqual
    "Expected single identity context" 
    (from_assumptions (Data.Set.singleton (Aspn f1a Fresh)))
    (remove_leaflets_in_requirements_of context1_one_a (Data.Set.singleton p01))
  assertEqual
    "Expected context with single identity type inside" 
    (context1_one_a_two_b_mod)
    (remove_leaflets_in_requirements_of context1_one_a_two_b (Data.Set.singleton p01))
  assertEqual
    "Expected context with single identity type inside and one modified" 
    (context2_one_a_one_b_mod)
    (remove_leaflets_in_requirements_of context2_one_a_two_b (Data.Set.singleton p01))

test_get_formulas_of_order = TestCase $ do
  assertEqual
    "No formulas of order 0" 
    (Set.empty)
    (get_formulas_of_order context_one_a_two_b 0)
  assertEqual
    "Two formulas of order 1" 
    (Set.fromList [f6a, f4])
    (get_formulas_of_order context_one_a_two_b 1)
  assertEqual
    "One formula of order 2" 
    (Set.fromList [f9])
    (get_formulas_of_order context_one_a_two_b 2)
  assertEqual
    "Two formulas of order 2" 
    (Set.fromList [f4a, f6b])
    (get_formulas_of_order context_one_a_two_b_mod 2)

test_myequal = TestCase $ do
  assertEqual
    "mequal"
    context2_one_a_one_b_mod
    context2_one_a_two_b


main :: IO ()
main = do
          x <- runTestTT $ TestList [
            --                             test_myequal,
            --                             test_from_assumptions,
                                     test_get_formula_for_target,
                                     test_get_literals_for_occurrences_atargets,
            --                             test_get_literals_for_occurrences_leaflets,
            --                             test_negative_only_in_leaflets,
                                     test_remove_leaflets_in_requirements_for_atom_of_context,
                                     test_remove_leaflets_in_requirements_of,
                                     test_get_formulas_of_order
                                    ]
          putStrLn $  showCounts x

