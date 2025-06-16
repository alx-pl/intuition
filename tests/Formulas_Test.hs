module Main where

import Data.TPTP
import Data.Text
import Data.Set
import qualified Data.Set as Set
import Data.Map
import qualified Data.Map as Map
import Formulas
import Test.HUnit

import FormulaExamples


two_args :: Set (FirstOrder Unsorted)
two_args = Data.Set.insert f01 (Data.Set.singleton f02)

three_args :: Set (FirstOrder Unsorted)
three_args = Data.Set.insert f1 (Data.Set.insert f01 (Data.Set.singleton f02))

singleton_f01 :: Set (FirstOrder Unsorted)
singleton_f01 = Data.Set.insert f01 (Data.Set.empty)

singleton_f1 :: Set (FirstOrder Unsorted)
singleton_f1 = Data.Set.singleton f1

two_from_f8 :: Set (FirstOrder Unsorted)
two_from_f8 = Data.Set.insert f03 (Data.Set.singleton f01)


test_get_order = TestCase $ do
  assertEqual
    "Order 0" 
    0 
    (get_order f01)
  assertEqual
    "Order 0" 
    0 
    (get_order f02)
  assertEqual
    "Order 1" 
    1 
    (get_order f1)
  assertEqual
    "Order 1" 
    1 
    (get_order f7)
  assertEqual
    "Order 2" 
    2
    (get_order (Connected f7 Implication f01))




test_get_target = TestCase $ assertEqual 
  "Should get target predicate" (Just p02) ( get_target f1 ) 

test_get_args = TestCase $ do
  assertEqual
    "Empty arguments" 
    Data.Set.empty 
    (get_args f2)
  assertEqual
    "Singleton argument" 
    singleton_f01 
    (get_args f1)
  assertEqual
    "Singleton argument from f9" 
    singleton_f1
    (get_args f9)
  assertEqual
    "Two arguments from f8" 
    two_from_f8
    (get_args f8)


test_args_tgt = TestCase $ do
  assertEqual
    "One arg and one target" 
    (singleton_f01, Just p02)
    (args_tgt f1)


test_combine_args_target = TestCase $ do
  assertEqual
    "Two arguments and a target"
    (Connected f02 Implication (Connected f01 Implication f02))
    (combine_args_target two_args p02)
  assertEqual
    "Three arguments and a target"
    (Connected f1 Implication (Connected f02 Implication (Connected f01 Implication f02)))
    (combine_args_target three_args p02)



test_remove_leaflets_in_requirements_for_atom_of_formula  = TestCase $ do
    assertEqual
      "Nothing changes"
      (f4,Map.empty)
      (remove_leaflets_in_requirements_for_atom_of_formula f4 p02)
    assertEqual
      "One change"
      (f4aa, Map.singleton p01 1)
      (remove_leaflets_in_requirements_for_atom_of_formula f4a p02)
    assertEqual
      "Remain two arguments with the same target"
      (f11a, Map.fromList [(p01,1), (p05,1), (p04,1)])
      (remove_leaflets_in_requirements_for_atom_of_formula f11 p02)


test_remove_frmargs_with_exception = TestCase $ do
  assertEqual
    "All arguments removed"
    (f02,Map.fromList [(p01,1), (p02,1), (p04,1), (p05,1)] )
    (remove_frmargs_with_exception f4 p03)
  assertEqual
    "The only argument not removed"
    (f3,Map.empty)
    (remove_frmargs_with_exception f3 p02)
  assertEqual
    "The only argument not removed, second time"
    (f1,Map.empty)
    (remove_frmargs_with_exception f1 p01)
  assertEqual
    "The only argument removed"
    (f02,Map.fromList [(p01,1)])
    (remove_frmargs_with_exception f1 p02)
  assertEqual
    "Non-atomic argument"
    (Connected f02 Implication f02, Map.fromList [(p04,1)])
    (remove_frmargs_with_exception f6b p02)


main = runTestTT $ TestList [test_get_order,
                             test_get_target, 
                             test_get_args,
                             test_args_tgt,
                             test_combine_args_target,
                             test_remove_leaflets_in_requirements_for_atom_of_formula,
                             test_remove_frmargs_with_exception
                            ]
