{-# LANGUAGE Trustworthy,
    TypeOperators, ScopedTypeVariables,
    DataKinds,
    TypeFamilies,
    MultiParamTypeClasses,
    FlexibleInstances, UndecidableInstances  #-}

module Type.BST.List (
    -- * List
    List(Nil, Cons), (.:.)
  , Length
  , MergeableL(MergeL, mergeL, MergeEL, mergeEL)
  , Sortable(Sort, sort)
  ) where

import GHC.TypeLits
import Type.BST.Proxy
import Type.BST.Item
import Type.BST.Compare

-- | Dependently-typed list.
data family List (as :: [*])
data instance List '[] = Nil
data instance List (a ': as) = Cons a (List as)
-- | Infix synonym for 'Cons'.
(.:.) :: a -> List as -> List (a ': as)
(.:.) = Cons
{-# INLINE (.:.) #-}
infixr 5 `Cons`, .:.
instance Show (List '[]) where
  show _ = "[]"
instance (Show a, Showlist (List as)) => Show (List (a ': as)) where
  show (x `Cons` xs) = "[" ++ show x ++ showlist xs
class Showlist (a :: *) where
  showlist :: a -> String
instance Showlist (List '[]) where
  showlist _ = "]"
instance (Show a, Showlist (List as)) => Showlist (List (a ': as)) where
  showlist (x `Cons` xs) = "," ++ show x ++ showlist xs

data family ListList (ass :: [[*]])
data instance ListList (as ': ass) = ConsCons (List as) (ListList ass)
data instance ListList '[] = NilNil
(.::.) :: List as -> ListList ass -> ListList (as ': ass)
(.::.) = ConsCons
{-# INLINE (.::.) #-}
infixr 5 `ConsCons`, .::.

type family Length (as :: [*]) :: Nat
type instance Length '[] = 0
type instance Length (_' ': as) = 1 + Length as

class MergeableL (as :: [*]) (bs :: [*]) where
  type MergeL as bs :: [*]
  mergeL :: List as -> List bs -> List (MergeL as bs)
  type MergeEL as bs :: [*]
  mergeEL :: List as -> List bs -> List (MergeEL as bs)
instance MergeableL '[] bs where
  type MergeL '[] bs = bs
  mergeL _ ys = ys
  {-# INLINE mergeL #-}
  type MergeEL '[] bs = bs
  mergeEL _ ys = ys
  {-# INLINE mergeEL #-}
instance MergeableL (a ': as) '[] where
  type MergeL (a ': as) '[] = a ': as
  mergeL xs _ = xs
  {-# INLINE mergeL #-}
  type MergeEL (a ': as) '[] = a ': as
  mergeEL xs _ = xs
  {-# INLINE mergeEL #-}
instance MergeableL' (Compare a b) (a ': as) (b ': bs) => MergeableL (a ': as) (b ': bs) where
  type MergeL (a ': as) (b ': bs) = MergeL' (Compare a b) (a ': as) (b ': bs)
  mergeL = mergeL' (Proxy :: Proxy (Compare a b))
  {-# INLINE mergeL #-}
  type MergeEL (a ': as) (b ': bs) = MergeEL' (Compare a b) (a ': as) (b ': bs)
  mergeEL = mergeEL' (Proxy :: Proxy (Compare a b))
  {-# INLINE mergeEL #-}
class MergeableL' (o :: Ordering) (as :: [*]) (bs :: [*]) where
  type MergeL' o as bs :: [*]
  mergeL' :: Proxy o -> List as -> List bs -> List (MergeL' o as bs)
  type MergeEL' o as bs :: [*]
  mergeEL' :: Proxy o -> List as -> List bs -> List (MergeEL' o as bs)
instance MergeableL as bs => MergeableL' EQ (Item key a ': as) (Item key b ': bs) where
  type MergeL' EQ (Item key a ': as) (Item key b ': bs) = Item key a ': MergeL as bs
  mergeL' _ (Cons x xs) (Cons _ ys) = Cons x (mergeL xs ys)
  {-# INLINABLE mergeL' #-}
  type MergeEL' EQ (Item key a ': as) (Item key b ': bs) = Item key (a, b) ': MergeEL as bs
  mergeEL' _ (Cons (Item x) xs) (Cons (Item y) ys) = Cons ((Item :: With key) (x, y)) (mergeEL xs ys)
  {-# INLINABLE mergeEL' #-}
instance MergeableL as bs => MergeableL' LT (a ': as) bs where
  type MergeL' LT (a ': as) bs = a ': MergeL as bs
  mergeL' _ (Cons x xs) ys = Cons x (mergeL xs ys)
  {-# INLINABLE mergeL' #-}
  type MergeEL' LT (a ': as) bs = a ': MergeEL as bs
  mergeEL' _ (Cons x xs) ys = Cons x (mergeEL xs ys)
  {-# INLINABLE mergeEL' #-}
instance MergeableL as bs => MergeableL' GT as (b ': bs) where
  type MergeL' GT as (b ': bs) = b ': MergeL as bs
  mergeL' _ xs (Cons y ys) = Cons y (mergeL xs ys)
  {-# INLINABLE mergeL' #-}
  type MergeEL' GT as (b ': bs) = b ': MergeEL as bs
  mergeEL' _ xs (Cons y ys) = Cons y (mergeEL xs ys)
  {-# INLINABLE mergeEL' #-}

class Revappable (as :: [*]) (bs :: [*]) where
  type Revapp as bs :: [*]
  revapp :: List as -> List bs -> List (Revapp as bs)
instance Revappable '[] bs where
  type Revapp '[] bs = bs
  revapp _ ys = ys
  {-# INLINE revapp #-}
instance Revappable as (a ': bs) => Revappable (a ': as) bs where
  type Revapp (a ': as) bs = Revapp as (a ': bs)
  revapp (Cons x xs) ys = revapp xs (x .:. ys)
  {-# INLINABLE revapp #-}

class Sortable (as :: [*]) where
  type Sort as :: [*]
  sort :: Sortable as => List as -> List (Sort as)
instance (Sequencesable as, Mergeallable (Sequences as)) => Sortable as where
  type Sort as = Mergeall (Sequences as)
  sort = mergeall . sequences
  {-# INLINE sort #-}

class Sequencesable (as :: [*]) where
  type Sequences as :: [[*]]
  sequences :: List as -> ListList (Sequences as)
instance Sequencesable '[] where
  type Sequences '[] = '[ '[] ]
  sequences xs = xs .::. NilNil
  {-# INLINE sequences #-}
instance Sequencesable '[a] where
  type Sequences '[a] = '[ '[a] ]
  sequences xs = xs .::. NilNil
  {-# INLINE sequences #-}
instance Sequencesable' (Compare a b) (a ': b ': cs) => Sequencesable (a ': b ': cs) where
  type Sequences (a ': b ': cs) = Sequences' (Compare a b) (a ': b ': cs)
  sequences xs = sequences' (Proxy :: Proxy (Compare a b)) xs
  {-# INLINE sequences #-}
class Sequencesable' (o :: Ordering) (as :: [*]) where
  type Sequences' o as :: [[*]]
  sequences' :: Proxy o -> List as -> ListList (Sequences' o as)
instance Sequencesable (a ': cs) => Sequencesable' EQ (a ': _' ': cs) where
  type Sequences' EQ (a ': _' ': cs) = Sequences (a ': cs)
  sequences' _ (x `Cons` _ `Cons` zs) = sequences (x .:. zs)
  {-# INLINE sequences' #-}
instance Ascendingable b '[a] cs => Sequencesable' LT (a ': b ': cs) where
  type Sequences' LT (a ': b ': cs) = Ascending b '[a] cs
  sequences' _ (x `Cons` y `Cons` zs) = ascending y (x .:. Nil) zs
  {-# INLINE sequences' #-}
instance Descendingable b '[a] cs => Sequencesable' GT (a ': b ': cs) where
  type Sequences' GT (a ': b ': cs) = Descending b '[a] cs
  sequences' _ (x `Cons` y `Cons` zs) = descending y (x .:. Nil) zs
  {-# INLINE sequences' #-}
class Descendingable (a :: *) (as :: [*]) (bs :: [*]) where
  type Descending a as bs :: [[*]]
  descending :: a -> List as -> List bs -> ListList (Descending a as bs)
instance Revappable as '[a] => Descendingable a as '[] where
  type Descending a as '[] = (a ': as) ': Sequences '[]
  descending x xs ys = (x .:. xs) .::. sequences ys
  {-# INLINABLE descending #-}
instance Descendingable' (Compare a b) a as (b ': bs) => Descendingable a as (b ': bs) where
  type Descending a as (b ': bs) = Descending' (Compare a b) a as (b ': bs)
  descending x xs ys = descending' (Proxy :: Proxy (Compare a b)) x xs ys
  {-# INLINE descending #-}
class Descendingable' (o :: Ordering) (a :: *) (as :: [*]) (bs :: [*]) where
  type Descending' o a as bs :: [[*]]
  descending' :: Proxy o -> a -> List as -> List bs -> ListList (Descending' o a as bs)
instance Descendingable a as bs => Descendingable' EQ a as (_'  ': bs) where
  type Descending' EQ a as (_'  ': bs) = Descending a as bs
  descending' _ x xs (Cons _ ys) = descending x xs ys
  {-# INLINABLE descending' #-}
instance Sequencesable bs => Descendingable' LT a as bs where
  type Descending' LT a as bs = (a ': as) ': Sequences bs
  descending' _ x xs ys = (x .:. xs) .::. sequences ys
  {-# INLINABLE descending' #-}
instance Descendingable b (a ': as) bs => Descendingable' GT a as (b  ': bs) where
  type Descending' GT a as (b  ': bs) = Descending b (a ': as) bs
  descending' _ x xs (Cons y ys) = descending y (x .:. xs) ys
  {-# INLINABLE descending' #-}
class Ascendingable (a :: *) (as :: [*]) (bs :: [*]) where
  type Ascending a as bs :: [[*]]
  ascending :: a -> List as -> List bs -> ListList (Ascending a as bs)
instance Revappable as '[a] => Ascendingable a as '[] where
  type Ascending a as '[] = Revapp as '[a] ': Sequences '[]
  ascending x xs ys = revapp xs (x .:. Nil) .::. sequences ys
  {-# INLINABLE ascending #-}
instance Ascendingable' (Compare a b) a as (b ': bs) => Ascendingable a as (b ': bs) where
  type Ascending a as (b ': bs) = Ascending' (Compare a b) a as (b ': bs)
  ascending x xs ys = ascending' (Proxy :: Proxy (Compare a b)) x xs ys
  {-# INLINE ascending #-}
class Ascendingable' (o :: Ordering) (a :: *) (as :: [*]) (bs :: [*]) where
  type Ascending' o a as bs :: [[*]]
  ascending' :: Proxy o -> a -> List as -> List bs -> ListList (Ascending' o a as bs)
instance Ascendingable b (a ': as) bs => Ascendingable' LT a as (b  ': bs) where
  type Ascending' LT a as (b  ': bs) = Ascending b (a ': as) bs
  ascending' _ x xs (Cons y ys) = ascending y (x .:. xs) ys
  {-# INLINABLE ascending' #-}
instance Ascendingable a as bs => Ascendingable' EQ a as (_'  ': bs) where
  type Ascending' EQ a as (_'  ': bs) = Ascending a as bs
  ascending' _ x xs (Cons _ ys) = ascending x xs ys
  {-# INLINABLE ascending' #-}
instance (Revappable as '[a], Sequencesable bs) => Ascendingable' GT a as bs where
  type Ascending' GT a as bs = Revapp as '[a] ': Sequences bs
  ascending' _ x xs ys = revapp xs (x .:. Nil) .::. sequences ys
  {-# INLINABLE ascending' #-}

class Mergeallable (ass :: [[*]]) where
  type Mergeall ass :: [*]
  mergeall :: ListList ass -> List (Mergeall ass)
instance Mergeallable '[as] where
  type Mergeall '[as] = as
  mergeall (ConsCons xs _) = xs
  {-# INLINE mergeall #-}
instance (Mergepairsable (as ': bs ': css), Mergeallable (Mergepairs (as ': bs ': css))) => Mergeallable (as ': bs ': css) where
  type Mergeall (as ': bs ': css) = Mergeall (Mergepairs (as ': bs ': css))
  mergeall xss = mergeall (mergepairs xss)
  {-# INLINABLE mergeall #-}
class Mergepairsable ass where
  type Mergepairs ass :: [[*]]
  mergepairs :: ListList ass -> ListList (Mergepairs ass)
instance Mergepairsable '[] where
  type Mergepairs '[] = '[]
  mergepairs xss = xss
  {-# INLINE mergepairs #-}
instance Mergepairsable '[as] where
  type Mergepairs '[as] = '[as]
  mergepairs xss = xss
  {-# INLINE mergepairs #-}
instance (MergeableL as bs, Mergepairsable css) => Mergepairsable (as ': bs ': css) where
  type Mergepairs (as ': bs ': css) = MergeL as bs ': Mergepairs css
  mergepairs (xs `ConsCons` ys `ConsCons` zss) = mergeL xs ys .::. mergepairs zss
  {-# INLINABLE mergepairs #-}
