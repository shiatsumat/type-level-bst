{-# LANGUAGE Safe, ExplicitNamespaces #-}

--------------------------------------------------------------------------------
-- |
-- Module: Type.BST
-- Copyright: (c) Yusuke Matsushita 2014
-- License: BSD3
-- Maintainer: Yusuke Matsushita
-- Stability: provisional
-- Portability: portable
--
-- An efficient implementation of type-level binary search trees and of dependently-typed extensible records and unions.
--
-- Comments on some type families and functions
-- contain the time complexity in the Big-O notation
-- on an assumption that @t@ (or @t'@) is balanced.
--
-- This library does not export a @proxy@ type.
-- Since @proxy@ is polymorphic, you can use your own proxy type,
-- and you can also use the one
-- in <http://hackage.haskell.org/package/tagged/docs/Data-Proxy.html Data.Proxy>.
--------------------------------------------------------------------------------

module Type.BST (
    -- * BST, Record and Union
    -- ** Data Types
    BST(Leaf, Node), Record(None, Triple), Union(Top, GoL, GoR)
    -- ** Conversion
  , Fromlistable(Fromlist, fromlist)
  , Fromsumable(fromsum)
  , Foldable(virtual), tolist, tosum, Tolist
    -- ** Accessible
  , Accessible(At), at, proj, inj, (!)
  , AccessibleF(), smap, supdate, adjust, update, (<$*>)
  , Contains, ContainedBy
    -- ** Searchable
  , Searchable(Smap, atX, projX, injX), (!?), SearchableF(smapX), supdateX, adjustX, updateX, (<$*?>)
    -- ** Unioncasable
  , Unioncasable, unioncase, UnioncasableX(unioncaseX)
    -- ** Inclusion
  , Includes, IncludedBy, shrink, expand
    -- ** Balancing
  , Balanceable(Balance, balance)
    -- ** Metamorphosis
  , Metamorphosable(metamorphose)
    -- ** Merging
  , Mergeable(Merge, merge, MergeE, mergeE)
    -- ** Insertion
  , Insertable(Insert, insert)
    -- ** Deletion
  , Deletable(Delete, delete)
    -- ** Min and Max
  , Minnable(Findmin, splitmin), findmin, deletemin
  , Maxable(Findmax, splitmax), findmax, deletemax
    -- * Item
  , Item(Item, value), type (|>)
  , With
  , newkey, item, (|>)
    -- * List
  , List(Nil, Cons), (.:.)
    -- * Sum
  , Sum(Head, Tail)
    -- * Comparison
  , Compare, LargestK(Largest), SmallestK(Smallest), CompareUser
    -- * Showtype
  , Showtype(showtype, showtypesPrec)
  ) where

import Type.BST.Showtype
import Type.BST.Item
import Type.BST.Compare
import Type.BST.List
import Type.BST.Sum
import Type.BST.BST
