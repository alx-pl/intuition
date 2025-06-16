module FormulaExamples where

import Data.List
import Data.TPTP
import Data.Text
import Data.Set
import qualified Data.Set as Set
import Data.Map 
import qualified Data.Map as Map
import Formulas


atom_a = Atom (pack "a")
atom_b = Atom (pack "b")
atom_c = Atom (pack "c")
atom_d = Atom (pack "d")
atom_e = Atom (pack "e")

p01 :: Literal
p01 = (Predicate (Defined atom_a) [])

p02 :: Literal
p02 = (Predicate (Defined atom_b) [])

p03 :: Literal
p03 = (Predicate (Defined atom_c) [])


p04 :: Literal
p04 = (Predicate (Defined atom_d) [])


p05 :: Literal
p05 = (Predicate (Defined atom_e) [])


f01 :: FirstOrder Unsorted
f01 = Atomic p01

f02 :: FirstOrder Unsorted
f02 = Atomic p02


f03 :: FirstOrder Unsorted
f03 = Atomic p03


f04 :: FirstOrder Unsorted
f04 = Atomic p04

f05 :: FirstOrder Unsorted
f05 = Atomic p05


-- a -> b
f1 :: FirstOrder Unsorted
f1 =  Connected f01 Implication f02

-- a -> a
f1a :: FirstOrder Unsorted
f1a =  Connected f01 Implication f01


-- a \/ b
f2 :: FirstOrder Unsorted
f2 =  Connected f01 Disjunction f02

-- b -> a
f3 :: FirstOrder Unsorted
f3 =  Connected f02 Implication f01



--[Connected (Atomic (Predicate (Defined (Atom "e")) [])) Implication
--           (Connected (Atomic (Predicate (Defined (Atom "d")) [])) Implication
--                      (Connected (Atomic (Predicate (Defined (Atom "b")) [])) Implication
--                                 (Connected (Atomic (Predicate (Defined (Atom "a")) [])) Implication
--                                            (Atomic (Predicate (Defined (Atom "b")) [])))))
-- e -> d -> b -> a -> b
f4 :: FirstOrder Unsorted
f4 =  Connected f05 Implication (Connected f04 Implication (Connected f02 Implication (Connected f01 Implication f02)))

-- (a -> b) -> e -> d -> b -> b 
f4a :: FirstOrder Unsorted
f4a =  Connected f1 Implication
                 (Connected f05 Implication
                            (Connected f04 Implication
                                       (Connected f02 Implication f02)))


--Connected (Connected (Atomic (Predicate (Defined (Atom "b")) [])) Implication
--                     (Atomic (Predicate (Defined (Atom "a")) []))) Implication
--          (Connected (Atomic (Predicate (Defined (Atom "e")) [])) Implication
--                     (Connected (Atomic (Predicate (Defined (Atom "d")) [])) Implication
--                                (Connected (Atomic (Predicate (Defined (Atom "b")) [])) Implication
--                                           (Atomic (Predicate (Defined (Atom "b")) [])))))
-- (b -> a) -> e -> d -> b -> b
f4b :: FirstOrder Unsorted
f4b =  Connected (Connected f02 Implication f01) Implication
                 (Connected f05 Implication
                            (Connected f04 Implication
                                       (Connected f02 Implication f02)))

-- e -> d -> b -> b
f4aa :: FirstOrder Unsorted
f4aa =  Connected f05 Implication
                  (Connected f04 Implication
                             (Connected f02 Implication f02))

-- a -> e -> d -> b -> b
f4ba :: FirstOrder Unsorted
f4ba = Connected f01 Implication
                 (Connected f05 Implication
                            (Connected f04 Implication
                                       (Connected f02 Implication f02)))


-- e -> d -> b -> a -> b
f4bb :: FirstOrder Unsorted
f4bb = Connected f05 Implication
                 (Connected f04 Implication
                            (Connected f02 Implication
                                       (Connected f01 Implication f02)))



-- Connected (Atomic (Predicate (Defined (Atom "e")) [])) Implication
--           (Connected (Atomic (Predicate (Defined (Atom "d")) [])) Implication
--                      (Connected (Atomic (Predicate (Defined (Atom "c")) [])) Implication
--                                 (Connected (Atomic (Predicate (Defined (Atom "a")) [])) Implication
--                                            (Atomic (Predicate (Defined (Atom "c")) [])))))]
f5 :: FirstOrder Unsorted
f5 =  Connected f05 Implication (Connected f04 Implication (Connected f03 Implication (Connected f01 Implication f03)))


-- Connected (Atomic (Predicate (Defined (Atom "b")) [])) Implication
--           (Connected (Atomic (Predicate (Defined (Atom "a")) [])) Implication
--                      (Atomic (Predicate (Defined (Atom "b")) []))),
-- b -> a -> b
f6 :: FirstOrder Unsorted
f6 =  Connected f02 Implication (Connected f01 Implication f02)

-- Connected (Atomic (Predicate (Defined (Atom "d")) [])) Implication
-- (Connected (Atomic (Predicate (Defined (Atom "b")) [])) Implication
--           (Connected (Atomic (Predicate (Defined (Atom "a")) [])) Implication
--                      (Atomic (Predicate (Defined (Atom "b")) []))))
-- d -> b -> a -> b
f6a :: FirstOrder Unsorted
f6a =  Connected f04 Implication f6


-- (a -> b) -> d -> b -> b
f6b :: FirstOrder Unsorted
f6b =  Connected f1 Implication
                 (Connected f04 Implication
                            (Connected f02 Implication f02))

-- (b -> a) -> d -> b -> b
f6c :: FirstOrder Unsorted
f6c =  Connected f3 Implication
                 (Connected f04 Implication
                            (Connected f02 Implication f02))


-- Connected (Atomic (Predicate (Defined (Atom "c")) [])) Implication
--           (Connected (Atomic (Predicate (Defined (Atom "b")) [])) Implication
--                      (Connected (Atomic (Predicate (Defined (Atom "a")) [])) Implication
--                                 (Atomic (Predicate (Defined (Atom "c")) []))))]
-- c -> b -> a -> c
f7 :: FirstOrder Unsorted
f7 =  Connected f03 Implication (Connected f02 Implication (Connected f01 Implication f03))


-- (Connected (Atomic (Predicate (Defined (Atom "c")) [])) Implication
--                     (Connected (Atomic (Predicate (Defined (Atom "a")) [])) Implication
--                                (Atomic (Predicate (Defined (Atom "c")) []))))
-- c -> a -> c
f8 :: FirstOrder Unsorted
f8 =  Connected f03 Implication (Connected f01 Implication f03)

-- Connected (Atomic (Predicate (Defined (Atom "d")) [])) Implication
-- (Connected (Atomic (Predicate (Defined (Atom "c")) [])) Implication
--                     (Connected (Atomic (Predicate (Defined (Atom "a")) [])) Implication
--                                (Atomic (Predicate (Defined (Atom "c")) []))))
-- d -> c -> a -> c
f8a :: FirstOrder Unsorted
f8a =  Connected f04 Implication f8

-- c -> b -> d
f8d :: FirstOrder Unsorted
f8d =  Connected f03 Implication (Connected f02 Implication f04)

-- (a -> b) -> a
f9 :: FirstOrder Unsorted
f9 =  Connected f1 Implication f01


-- (b -> a) -> a
f10 :: FirstOrder Unsorted
f10 =  Connected f3 Implication f01


-- (a -> b) -> (e -> d -> b -> b) -> c
f11  :: FirstOrder Unsorted
f11 = Connected f1 Implication
                (Connected f4aa Implication f03)


-- b -> (b -> b) -> c
f11a  :: FirstOrder Unsorted
f11a = Connected (Connected f02 Implication f02) Implication
                (Connected f02 Implication f03)

f12  :: FirstOrder Unsorted
f12 = Connected f04 Implication f01
