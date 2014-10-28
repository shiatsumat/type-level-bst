{-# LANGUAGE Safe,
    TypeOperators, BangPatterns,
    DataKinds,
    TypeFamilies,
    FlexibleContexts, FlexibleInstances #-}

module Type.BST.Sum (
    -- * Sum
    Sum(Head, Tail)
  ) where

-- | Dependently-typed sum.
data family Sum (as :: [*])
data instance Sum '[]
data instance Sum (a ': as) = Head a | Tail (Sum as)
instance Show (Sum '[]) where
  show !_ = undefined
instance (Show a, Show (Sum as)) => Show (Sum (a ': as)) where
  showsPrec p (Head x) = showParen (p > 10) $ showString "<Sum> " . showsPrec 11 x
  showsPrec p (Tail s) = showsPrec p s
