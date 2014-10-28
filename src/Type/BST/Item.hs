{-# LANGUAGE Safe,
    TypeOperators, ScopedTypeVariables,
    PolyKinds,
    RankNTypes, TypeFamilies,
    MultiParamTypeClasses #-}

module Type.BST.Item (
    -- * Item
    Item(Item, value), type (|>)
  , With
  , newkey, item, (|>)
  ) where

import Type.BST.Proxy
import Type.BST.Showtype

-- | 'Item' used in 'BST'.
newtype Item key a = Item {value :: a}
-- | When @x@ has a type @T@,
-- @('Item' :: 'With' key) x@ works as @'Item' x :: 'Item' key T@.
--
-- Using 'With', you can avoid writing @T@.
type With key = forall a. a -> Item key a
-- | Infix synonym for 'Item'.
type (|>) = Item
instance (Showtype key, Show a) => Show (Item key a) where
  showsPrec p (Item x) = showParen (p > 10) $
    (\s -> "<" ++ showtype (Proxy :: Proxy key) ++ "> " ++ s) .
    showsPrec 11 x

-- | Give a new key to an 'Item'.
newkey :: Item key a -> proxy key' -> Item key' a
newkey (Item x) _ = Item x
{-# INLINE newkey #-}
-- | Make an 'Item' setting a key with @proxy@.
item :: proxy key -> a -> Item key a
item _ = Item
{-# INLINE item #-}
-- | Infix synonym for 'item'.
(|>) :: proxy key -> a -> Item key a
(|>) = item
{-# INLINE (|>) #-}
infixr 6 |>
