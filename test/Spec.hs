{-# OPTIONS_GHC -Wno-orphans #-}

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck as QC
import Prelude (Eq (..), IO, Int, length, show, (!!), ($), (<>))

import Nominal
import OrbitList (repeatRationals, size)
import SpecMap
import SpecPermutable
import SpecSet
import SpecUtils ()

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "main" [setTests, mapTests, countingTests, qcTests, permutableTests]

-- Verifying that the number of orbits is correct. Up to length 7, because
-- length 8 and longer take at least one second.
countingTests :: TestTree
countingTests = testGroup "OrbitList" [testCase ("count " <> show n) $ length (OrbitList.size (repeatRationals n)) @?= (a000670 !! n) | n <- [0 .. 7]]

-- A000670: Ordered Bell numbers or Fubini numbers
a000670 :: [Int]
a000670 = [1, 1, 3, 13, 75, 541, 4683, 47293, 545835, 7087261, 102247563, 1622632573, 28091567595]

-- TODO: Add more quickcheck tests
qcTests :: TestTree
qcTests = testGroup "QuickCheck" [QC.testProperty "all atoms in same orbit" $ \p q -> toOrbit (p :: Atom) == toOrbit (q :: Atom)]
