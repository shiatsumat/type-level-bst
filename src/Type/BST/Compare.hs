{-# LANGUAGE Trustworthy,
    TypeOperators,
    PolyKinds, DataKinds,
    TypeFamilies,
    UndecidableInstances #-}

module Type.BST.Compare (
    -- * Comparison
    Compare, LargestK(Largest), SmallestK(Smallest), CompareUser
  ) where

import GHC.TypeLits
import Type.BST.Item

type family (a :: Ordering) $$ (b :: Ordering) :: Ordering
type instance LT $$ b = LT
type instance GT $$ b = GT
type instance EQ $$ b = b
infixl 0 $$

-- | The largest type (and kind) on 'Compare'.
data LargestK = Largest
-- | The smallest type (and kind) on 'Compare'.
data SmallestK = Smallest

-- | Compare two types.
type family Compare (a :: k) (b :: k') :: Ordering where
  Compare Largest Largest = EQ
  Compare _' Largest = LT
  Compare Largest _' = GT
  Compare Smallest Smallest = EQ
  Compare _' Smallest = GT
  Compare Smallest _' = LT
  Compare False False = EQ
  Compare False True = LT
  Compare True False = GT
  Compare True True = EQ
  Compare LT LT = EQ
  Compare LT EQ = LT
  Compare LT GT = LT
  Compare EQ LT = GT
  Compare EQ EQ = EQ
  Compare EQ GT = LT
  Compare GT LT = GT
  Compare GT EQ = GT
  Compare GT GT = EQ
  Compare m n = CmpNat m n
  Compare s t = CmpSymbol s t
  Compare Nothing Nothing = EQ
  Compare Nothing (Just b) = LT
  Compare (Just a) Nothing = GT
  Compare (Just a) (Just b) = Compare a b
  Compare (Left _') (Right _'') = LT
  Compare (Right _') (Left _'') = GT
  Compare (Left a) (Left b) = Compare a b
  Compare (Right a) (Right b) = Compare a b
  Compare '[] '[] = EQ
  Compare '[] (b ': bs) = LT
  Compare (a ': as) '[] = GT
  Compare (a ': as) (b ': bs) = Compare a b $$ Compare as bs
  Compare '(a1, a2) '(b1, b2) = Compare a1 b1 $$ Compare a2 b2
  Compare '(a1, a2, a3) '(b1, b2, b3) = Compare a1 b1 $$ Compare a2 b2 $$ Compare a3 b3
  Compare '(a1, a2, a3, a4) '(b1, b2, b3, b4) = Compare a1 b1 $$ Compare a2 b2 $$ Compare a3 b3 $$ Compare a4 b4
  Compare '(a1, a2, a3, a4, a5) '(b1, b2, b3, b4, b5) = Compare a1 b1 $$ Compare a2 b2 $$ Compare a3 b3 $$ Compare a4 b4 $$ Compare a5 b5
  Compare '(a1, a2, a3, a4, a5, a6) '(b1, b2, b3, b4, b5, b6) = Compare a1 b1 $$ Compare a2 b2 $$ Compare a3 b3 $$ Compare a4 b4 $$ Compare a5 b5 $$ Compare a6 b6
  Compare '(a1, a2, a3, a4, a5, a6, a7) '(b1, b2, b3, b4, b5, b6, b7) = Compare a1 b1 $$ Compare a2 b2 $$ Compare a3 b3 $$ Compare a4 b4 $$ Compare a5 b5 $$ Compare a6 b6 $$ Compare a7 b7
  Compare '(a1, a2, a3, a4, a5, a6, a7, a8) '(b1, b2, b3, b4, b5, b6, b7, b8) = Compare a1 b1 $$ Compare a2 b2 $$ Compare a3 b3 $$ Compare a4 b4 $$ Compare a5 b5 $$ Compare a6 b6 $$ Compare a7 b7 $$ Compare a8 b8
  Compare '(a1, a2, a3, a4, a5, a6, a7, a8, a9) '(b1, b2, b3, b4, b5, b6, b7, b8, b9) = Compare a1 b1 $$ Compare a2 b2 $$ Compare a3 b3 $$ Compare a4 b4 $$ Compare a5 b5 $$ Compare a6 b6 $$ Compare a7 b7 $$ Compare a8 b8 $$ Compare a9 b9
  Compare '(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) '(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10) = Compare a1 b1 $$ Compare a2 b2 $$ Compare a3 b3 $$ Compare a4 b4 $$ Compare a5 b5 $$ Compare a6 b6 $$ Compare a7 b7 $$ Compare a8 b8 $$ Compare a9 b9 $$ Compare a10 b10
  Compare (Item key a) (Item key' b) = Compare key key'
  Compare a b = CompareUser a b

-- | Compare two types. Users can add instances.
type family CompareUser (a :: k) (b :: k') :: Ordering
