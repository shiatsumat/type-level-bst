{-# LANGUAGE Trustworthy,
    TypeOperators, ScopedTypeVariables, BangPatterns,
    PolyKinds, DataKinds, ConstraintKinds,
    RankNTypes, TypeFamilies,
    MultiParamTypeClasses, FunctionalDependencies,
    FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Type.BST.BST (
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
  ) where

import Data.Maybe
import GHC.TypeLits
import Type.BST.Proxy
import Type.BST.Item
import Type.BST.Compare
import Type.BST.List
import Type.BST.Sum

{- Data Types -}

-- | Type-level binary search tree used as an \"ordered map\".
--
-- @a@ is supposed to be a proper type @'Item' key val@ of the kind @*@,
-- where @key@ is a key type of an arbitrary kind
-- @val@ is a value type of the kind @*@.
-- All 'Item's in a 'BST' should have different keys.
--
-- Type inference works very well with 'BST's, and value types can naturally be /polymorphic/.
--
-- If you want to build a new 'BST', use 'Fromlist', whose result is guaranteed to be balanced.
data BST a = Leaf | Node (BST a) a (BST a)

-- | Dependently-typed record, or an /extensible/ record.
-- If you want to build a new 'Record', use 'fromlist'.
data family Record (s :: BST *)
data instance Record Leaf = None
data instance Record (Node l a r) = Triple (Record l) a (Record r)
instance (Foldable s, Show (List (Tolist s))) => Show (Record s) where
  showsPrec p rc = showParen (p > 10) $ showString "<Record> " . showsPrec 11 (tolist rc)

-- | Dependently-typed union, or an /extensible/ union.
-- If you want to build a new 'Union', use 'inj' or 'fromsum'.
data family Union (s :: BST *)
data instance Union Leaf
data instance Union (Node l a r) = Top a | GoL (Union l) | GoR (Union r)
instance Show (Union Leaf) where
  show !_ = undefined
instance (Show a, Show (Union l), Show (Union r)) => Show (Union (Node l a r)) where
  showsPrec p (Top x) = showParen (p > 10) $ showString "<Union> " . showsPrec 11 x
  showsPrec p (GoL l) = showsPrec p l
  showsPrec p (GoR r) = showsPrec p r

{- Conversion -}

class Fromlistable (as :: [*]) where
  -- | /O(n log n)/. Convert from a type-level list to a 'BST'.
  -- If @as@ has two or more 'Item's that have the same key, only the first one will be added to the result.
  --
  -- If @as@ and @bs@ have the same items (just in different orders), @Fromlist as == Fromlist bs@.
  --
  -- Example:
  --
  -- >>> type T = Fromlist [0 |> Bool, 4 |> String, 2 |> Int, 3 |> Double, 1 |> Char]
  -- >>> :kind! T
  -- T :: BST *
  -- = 'Node
  --     ('Node 'Leaf (Item 0 Bool) ('Node 'Leaf (Item 1 Char) 'Leaf))
  --     (Item 2 Int)
  --     ('Node ('Node 'Leaf (Item 3 Double) 'Leaf) (Item 4 [Char]) 'Leaf)
  -- >>> :kind! Tolist T
  -- Tolist T :: [*]
  -- = '[Item 0 Bool, Item 1 Char, Item 2 Int, Item 3 Double,
  --     Item 4 [Char]]
  type Fromlist as :: BST *
  -- | /O(n log n)/. Convert from a 'List' to a 'Record'.
  --
  -- If @rc@ and @rc'@ have the same items (just in different orders), @fromlist rc == fromlist rc'@.
  --
  -- Example (where @T@ is a type defined in the previous example):
  --
  -- >>> let rct = fromlist $ p3 |> 3 .:. p0 |> True .:. p2 |> 10 .:. p4 |> "wow" .:. p1 |> 'a' .:. Nil
  -- >>> rct
  -- <Record> [<0> True,<1> 'a',<2> 10,<3> 3,<4> "wow"]
  -- >>> :type rct
  -- rct
  --   :: (Num a1, Num a) =>
  --      Record
  --        ('Node
  --           ('Node 'Leaf (Item 0 Bool) ('Node 'Leaf (Item 1 Char) 'Leaf))
  --           (Item 2 a1)
  --           ('Node ('Node 'Leaf (Item 3 a) 'Leaf) (Item 4 [Char]) 'Leaf))
  -- >>> :type rct :: Record T
  -- rct :: Record T :: Record T
  fromlist :: List as -> Record (Fromlist as)
instance (Sortable as, Fromslistable (Sort as)) => Fromlistable as where
  type Fromlist as = Fromslist (Sort as)
  fromlist xs = fromslist (sort xs)
  {-# INLINE fromlist #-}
type Fromslist as = Fromslist' True (Length as) 0 as '[]
type Fromslistable as = Fromslistable' True (Length as) 0 as '[]
fromslist :: Fromslistable as => List as -> Record (Fromslist as)
fromslist (xs :: List as) = fromslist' (Proxy :: Proxy True) (Proxy :: Proxy (Length as)) (Proxy :: Proxy 0) xs Nil
{-# INLINE fromslist #-}
type FromslistR as = Fromslist' False (Length as) 0 as '[]
type FromslistableR as = Fromslistable' False (Length as) 0 as '[]
fromslistR :: FromslistableR as => List as -> Record (FromslistR as)
fromslistR (xs :: List as) = fromslist' (Proxy :: Proxy False) (Proxy :: Proxy (Length as)) (Proxy :: Proxy 0) xs Nil
{-# INLINE fromslistR #-}
class Fromslistable' (fw :: Bool) (m :: Nat) (n :: Nat) (as :: [*]) (bs :: [*]) where
  type Fromslist' fw m n as bs :: BST *
  fromslist' :: Proxy fw -> Proxy m -> Proxy n -> List as -> List bs -> Record (Fromslist' fw m n as bs)
instance Fromslistable' fw 0 0 '[] '[] where
  type Fromslist' fw 0 0 '[] '[] = Leaf
  fromslist' _ _ _ _ _ = None
  {-# INLINE fromslist' #-}
instance Fromslistable' fw 0 1 '[] '[b] where
  type Fromslist' fw 0 1 '[] '[b] = Node Leaf b Leaf
  fromslist' _ _ _ _ (Cons y _) = Triple None y None
  {-# INLINE fromslist' #-}
instance Fromslistable'' (m <=? n + 1) fw m n (a ': as) bs => Fromslistable' fw m n (a ': as) bs where
  type Fromslist' fw m n (a ': as) bs = Fromslist'' (m <=? n + 1) fw m n (a ': as) bs
  fromslist' = fromslist'' (Proxy :: Proxy (m <=? n + 1))
  {-# INLINE fromslist' #-}
class Fromslistable'' (b :: Bool) (fw :: Bool) (m :: Nat) (n :: Nat) (as :: [*]) (bs :: [*]) where
  type Fromslist'' b fw m n as bs :: BST *
  fromslist'' :: Proxy b -> Proxy fw -> Proxy m -> Proxy n -> List as -> List bs -> Record (Fromslist'' b fw m n as bs)
instance (Fromslistable as, FromslistableR bs) => Fromslistable'' True True m n (a ': as) bs where
  type Fromslist'' True True m n (a ': as) bs = Node (FromslistR bs) a (Fromslist as)
  fromslist'' _ _ _ _ (Cons x xs) ys = Triple (fromslistR ys) x (fromslist xs)
  {-# INLINABLE fromslist'' #-}
instance (FromslistableR as, Fromslistable bs) => Fromslistable'' True False m n (a ': as) bs where
  type Fromslist'' True False m n (a ': as) bs = Node (FromslistR as) a (Fromslist bs)
  fromslist'' _ _ _ _ (Cons x xs) ys = Triple (fromslistR xs) x (fromslist ys)
  {-# INLINABLE fromslist'' #-}
instance Fromslistable' fw (m - 1) (n + 1) as (a ': bs) => Fromslistable'' False fw m n (a ': as) bs where
  type Fromslist'' False fw m n (a ': as) bs = Fromslist' fw (m - 1) (n + 1) as (a ': bs)
  fromslist'' _ _ _ _ (Cons x xs) ys = fromslist' (Proxy :: Proxy fw) (Proxy :: Proxy (m - 1)) (Proxy :: Proxy (n + 1)) xs (x .:. ys)
  {-# INLINABLE fromslist'' #-}

class Fromsumable (as :: [*]) where
  -- | /O(n)/. Convert from a 'Sum' to a 'Union'.
  fromsum :: Sum as -> Union (Fromlist as)
instance Fromsumable' as (Fromlist as) => Fromsumable as where
  fromsum = fromsum'
  {-# INLINE fromsum #-}
class Fromsumable' (as :: [*]) (t :: BST *) where
  fromsum' :: Sum as -> Union t
instance Fromsumable' '[] t where
  fromsum' !_ = undefined
  {-# INLINE fromsum' #-}
instance (Contains t key a, Fromsumable' as t) => Fromsumable' (key |> a ': as) t where
  fromsum' (Head (Item x)) = inj (Proxy :: Proxy key) x
  fromsum' (Tail s) = fromsum' s
  {-# INLINABLE fromsum' #-}

class Foldable (t :: BST *) where
  type Tolist' t (as :: [*]) :: [*]
  tolist' :: Record t -> List as -> List (Tolist' t as)
  tosum' :: Either (Sum as) (Union t) -> Sum (Tolist' t as)
  -- | Build a 'Record' with empty 'Item's.
  virtual :: Record t
-- | /O(n)/. Convert from a 'BST' to a type-level sorted list.
type family Tolist (t :: BST *) :: [*]
type instance Tolist t = Tolist' t '[]
-- | /O(n)/. Convert from a 'Record' to a 'List'.
tolist :: Foldable t => Record t -> List (Tolist t)
tolist rc = tolist' rc Nil
{-# INLINE tolist #-}
-- | /O(n)/. Convert from a 'Union' to a 'Sum'.
tosum :: Foldable t => Union t -> Sum (Tolist t)
tosum (u :: Union t) = tosum' (Right u :: Either (Sum '[]) (Union t))
{-# INLINE tosum #-}
instance Foldable Leaf where
  type Tolist' Leaf as = as
  tolist' _ xs = xs
  {-# INLINE tolist' #-}
  tosum' (Right !_) = undefined
  tosum' (Left s) = s
  {-# INLINE tosum' #-}
  virtual = None
  {-# INLINE virtual #-}
instance (Foldable l, Foldable r) => Foldable (Node l a r) where
  type Tolist' (Node l a r) as = Tolist' l (a ': Tolist' r as)
  tolist' (Triple l x r) xs = tolist' l (x .:. tolist' r xs)
  {-# INLINABLE tolist' #-}
  tosum' (Right (Top x) :: Either (Sum as) (Union (Node l a r))) = tosum' (Left $ Head x :: Either (Sum (a ': Tolist' r as)) (Union l))
  tosum' (Right (GoL u) :: Either (Sum as) (Union (Node l a r))) = tosum' (Right u :: Either (Sum (a ': Tolist' r as)) (Union l))
  tosum' (Right (GoR u) :: Either (Sum as) (Union (Node l a r))) = tosum' (Left $ Tail $ tosum' (Right u :: Either (Sum as) (Union r)) :: Either (Sum (a ': Tolist' r as)) (Union l))
  tosum' (Left s :: Either (Sum as) (Union (Node l a r))) = tosum' (Left $ Tail $ tosum' (Left s :: Either (Sum as) (Union r)) :: Either (Sum (a ': Tolist' r as)) (Union l))
  {-# INLINABLE tosum' #-}
  virtual = Triple (virtual :: Record l) (error "Type.BST.BST.virtual: an empty item") (virtual :: Record r)
  {-# INLINABLE virtual #-}

{- Accessible -}

-- | @t@ has an 'Item' with @key@.
class Searchable t key (At t key) => Accessible (t :: BST *) (key :: k)  where
  -- | /O(log n)/. Find the type at @key@.
  type At t key :: *
instance Accessible' (Compare key key') (Node l (key' |> a) r) key => Accessible (Node l (key' |> a) r) key where
  type At (Node l (key' |> a) r) key = At' (Compare key key') (Node l (key' |> a) r) key
class Searchable' o t key (At' o t key) => Accessible' (o :: Ordering) (t :: BST *) (key :: k) where
  type At' o t key :: *
instance Accessible' EQ (Node _' (key |> a) _'') key where
  type At' EQ (Node _' (key |> a) _'') key = a
instance Accessible l key => Accessible' LT (Node l _' _'') key where
  type At' LT (Node l _' _'') key = At l key
instance Accessible r key => Accessible' GT (Node _' _'' r) key where
  type At' GT (Node _' _'' r) key = At r key
-- | /O(log n)/. Get the value at @key@ in a 'Record'.
--
-- A special version of 'atX' with a tighter context and without 'Maybe'.
at :: Accessible t key => Record t -> proxy key -> At t key
at rc p = fromJust $ atX rc p
{-# INLINE at #-}
-- | /O(log n)/. Get the value at @key@ in a 'Union'.
--
-- A special version of 'projX' with a tighter context.
proj :: Accessible t key => Union t -> proxy key -> Maybe (At t key)
proj = projX
{-# INLINE proj #-}
-- | /O(log n)/. Injection to a dependently-typed union. One way to build a 'Union'.
--
-- A special version of 'injX' with a tighter context and without 'Maybe'.
--
-- >Just (inj p x) == injX p x
inj :: Accessible t key => proxy key -> At t key -> Union t
inj p x = fromJust $ injX p x
{-# INLINE inj #-}
-- | Infix synonym for 'at'.
(!) :: Accessible t key => Record t -> proxy key -> At t key
(!) = at
{-# INLINE (!) #-}
infixl 9 !

-- | When f is either 'Record' or 'Union',
--
-- >AccessibleF f t key == Accessible t key
class (Accessible t key, SearchableF f t key (At t key)) => AccessibleF (f :: BST * -> *) (t :: BST *) (key :: k)
instance Accessible t key => AccessibleF Record t key
instance Accessible t key => AccessibleF Union t key
-- | /O(log n)/. 'smap' stands for \"super-map\".
-- Change the value of the 'Item' at @key@ in @f t@ by applying the function @At t key -> b@.
-- A special version of 'smapX' with a tighter context.
smap :: AccessibleF f t key => proxy key -> (At t key -> b) -> f t -> f (Smap t key (At t key) b)
smap = smapX
{-# INLINE smap #-}
-- | /O(log n)/. 'supdate' stands for \"super-update\".
-- A special version of 'supdateX' with a tighter context.
--
-- >supdate p x == smap p (const x)
supdate :: AccessibleF f t key => proxy key -> b -> f t -> f (Smap t key (At t key) b)
supdate = supdateX
{-# INLINE supdate #-}
-- | /O(log n)/. A special version of 'smap' that does not change the value type.
-- A special version of 'adjustX' with a tighter context.
adjust :: AccessibleF f t key => proxy key -> (At t key -> At t key) -> f t -> f t
adjust = adjustX
{-# INLINE adjust #-}
-- | /O(log n)/. A special version of 'supdate' that does not change the value type.
-- A special version of 'updateX' with a tighter context.
--
-- >update p x == adjust p (const x)
update :: AccessibleF f t key => proxy key -> At t key -> f t -> f t
update p x = adjust p (const x)
{-# INLINE update #-}
-- | Infix operator that works almost like 'smap'.
--
-- > (p |> f) <$*> c = smap p f c
(<$*>) :: AccessibleF f t key => key |> (At t key -> b) -> f t -> f (Smap t key (At t key) b)
(<$*>) (Item f :: key |> (At t key -> b)) (c :: f t) = smap (Proxy :: Proxy key) f c
{-# INLINE (<$*>) #-}
infixl 4 <$*>

-- >Contains t key k == (Accessible t key, a ~ At t key)
class (Accessible t key, a ~ At t key) => Contains (t :: BST *) (key :: k) (a :: *) | t key -> a
instance (Accessible t key, a ~ At t key) => Contains t key a

-- >ContainedBy key a t == (Accessible t key, a ~ At t key)
class Contains t key a => ContainedBy (key :: k) (a :: *) (t :: BST *) | t key -> a
instance Contains t key a => ContainedBy key a t

{- Searchable -}

-- | @t@ /may/ have an 'Item' with a key type @key@ and a value type @a@.
--
-- When @'Accessible' t key@, @a@ should be @'At' t key@,
-- but when not, @a@ can be any type.
class Smap t key a a ~ t => Searchable (t :: BST *) (key :: k) (a :: *) where
  -- | /O(log n)/. 'at' with a looser context and 'Maybe'.
  atX :: Record t -> proxy key -> Maybe a
  -- | /O(log n)/. 'proj' with a looser context.
  projX :: Union t -> proxy key -> Maybe a
  -- | /O(log n)/. 'inj' with a looser context and 'Maybe'.
  injX :: proxy key -> a -> Maybe (Union t)
  -- | /O(log n)/. Change the value type of an 'Item' at 'key' from 'a' to 'b'.
  type Smap t key a (b :: *) :: BST *
  smapXR :: proxy key -> (a -> b) -> Record t -> Record (Smap t key a b)
  smapXU :: proxy key -> (a -> b) -> Union t -> Union (Smap t key a b)
-- | Infix synonym for 'atX'.
(!?) :: Searchable t key a => Record t -> proxy key -> Maybe a
(!?) = atX
{-# INLINE (!?) #-}
instance Searchable Leaf _' _'' where
  atX _ _ = Nothing
  {-# INLINE atX #-}
  projX _ _ = Nothing
  {-# INLINE projX #-}
  injX _ _ = Nothing
  {-# INLINE injX #-}
  type Smap Leaf key a b = Leaf
  smapXR _ _ _ = None
  {-# INLINE smapXR #-}
  smapXU _ _ u = u
  {-# INLINE smapXU #-}
instance Searchable' (Compare key key') (Node l (key' |> c) r) key a => Searchable (Node l (key' |> c) r) key a where
  atX rc _ = atX' (Proxy :: Proxy '(Compare key key', key)) rc
  {-# INLINE atX #-}
  projX u _ = projX' (Proxy :: Proxy '(Compare key key', key)) u
  {-# INLINE projX #-}
  injX _ x = injX' (Proxy :: Proxy '(Compare key key', key)) x
  {-# INLINE injX #-}
  type Smap (Node l (key' |> c) r) key a b = Smap' (Compare key key') (Node l (key' |> c) r) key a b
  smapXR _ = smapXR' (Proxy :: Proxy '(Compare key key', key))
  {-# INLINE smapXR #-}
  smapXU _ = smapXU' (Proxy :: Proxy '(Compare key key', key))
  {-# INLINE smapXU #-}
class Smap' o t key a a ~ t => Searchable' (o :: Ordering) (t :: BST *) (key :: k) (a :: *) where
  atX' :: Proxy '(o, key) -> Record t -> Maybe a
  projX' :: Proxy '(o, key) -> Union t -> Maybe a
  injX' :: Proxy '(o, key) -> a -> Maybe (Union t)
  type Smap' o t key a (b :: *) :: BST *
  smapXR' :: Proxy '(o, key) -> (a -> b) -> Record t -> Record (Smap' o t key a b)
  smapXU' :: Proxy '(o, key) -> (a -> b) -> Union t -> Union (Smap' o t key a b)
instance Searchable' EQ (Node _' (key |> a) _'') key a where
  atX' _ (Triple _ (Item x) _) = Just x
  {-# INLINE atX' #-}
  projX' _ (Top (Item x)) = Just x
  projX' _ _ = Nothing
  {-# INLINE projX' #-}
  injX' _ x = Just $ Top $ (Item :: With key) x
  {-# INLINE injX' #-}
  type Smap' EQ (Node l (key |> a) r) key a b = Node l (key |> b) r
  smapXR' _ f (Triple l (Item x) r) = Triple l (Item $ f x) r
  {-# INLINE smapXR' #-}
  smapXU' _ f (Top (Item x)) = Top $ Item $ f x
  smapXU' _ _ (GoL l) = GoL l
  smapXU' _ _ (GoR r) = GoR r
  {-# INLINE smapXU' #-}
instance Searchable l key a => Searchable' LT (Node l _' _'') key a where
  atX' _ (Triple l _ _) = atX l (Proxy :: Proxy key)
  {-# INLINABLE atX' #-}
  projX' _ (GoL u) = projX u (Proxy :: Proxy key)
  projX' _ _ = Nothing
  {-# INLINABLE projX' #-}
  injX' _ x = fmap GoL $ injX (Proxy :: Proxy key) x
  {-# INLINABLE injX' #-}
  type Smap' LT (Node l c r) key a b = Node (Smap l key a b) c r
  smapXR' _ f (Triple l x r) = Triple (smapXR (Proxy :: Proxy key) f l) x r
  {-# INLINABLE smapXR' #-}
  smapXU' _ f (GoL l) = GoL $ smapXU (Proxy :: Proxy key) f l
  smapXU' _ _ (GoR r) = GoR r
  smapXU' _ _ (Top x) = Top x
  {-# INLINABLE smapXU' #-}
instance Searchable r key a => Searchable' GT (Node _' _'' r) key a where
  atX' _ (Triple _ _ r) = atX r (Proxy :: Proxy key)
  {-# INLINABLE atX' #-}
  projX' _ (GoR u) = projX u (Proxy :: Proxy key)
  projX' _ _ = Nothing
  {-# INLINABLE projX' #-}
  injX' _ x = fmap GoR $ injX (Proxy :: Proxy key) x
  {-# INLINABLE injX' #-}
  type Smap' GT (Node l c r) key a b = Node l c (Smap r key a b)
  smapXR' _ f (Triple l x r) = Triple l x (smapXR (Proxy :: Proxy key) f r)
  {-# INLINABLE smapXR' #-}
  smapXU' _ f (GoR r) = GoR $ smapXU (Proxy :: Proxy key) f r
  smapXU' _ _ (GoL l) = GoL l
  smapXU' _ _ (Top x) = Top x
  {-# INLINABLE smapXU' #-}

-- | When f is either 'Record' or 'Union',
--
-- >SearchableF f t key == Searchable t key
class Searchable t key a => SearchableF (f :: BST * -> *) (t :: BST *) (key :: k) (a :: *) where
  -- | /O(log n)/. 'smap' with a looser context.
  smapX :: proxy key -> (a -> b) -> f t -> f (Smap t key a b)
-- | /O(log n)/. 'supdate' with a looser context.
supdateX :: SearchableF f t key (At t key) => proxy key -> b -> f t -> f (Smap t key (At t key) b)
supdateX (p :: proxy key) (x :: b) (c :: f t) = smapX p (const x :: At t key -> b) c
{-# INLINE supdateX #-}
-- | /O(log n)/. 'adjust' with a looser context.
adjustX :: SearchableF f t key a => proxy key -> (a -> a) -> f t -> f t
adjustX = smapX
{-# INLINE adjustX #-}
-- | /O(log n)/. 'update' with a looser context.
updateX :: SearchableF f t key a =>  proxy key -> a -> f t -> f t
updateX p x = adjustX p (const x)
{-# INLINE updateX #-}
-- | Infix operator that works almost like 'smapX'.
--
-- > (p |> f) <$*?> c = smapX p f cz
(<$*?>) :: SearchableF f t key a => key |> (a -> b) -> f t -> f (Smap t key a b)
(<$*?>) (Item f :: key |> (a -> b)) = smapX (Proxy :: Proxy key) f
{-# INLINE (<$*?>) #-}
infixl 4 <$*?>
instance Searchable t key a => SearchableF Record t key a where
  smapX = smapXR
  {-# INLINE smapX #-}
instance Searchable t key a => SearchableF Union t key a where
  smapX = smapXU
  {-# INLINE smapX #-}

{- Unioncasable -}

class UnioncasableX t t' res => Unioncasable (t :: BST *) (t' :: BST *) (res :: *)
instance Unioncasable Leaf t' res
instance (Contains t' key (a -> res), Unioncasable l t' res, Unioncasable r t' res) => Unioncasable (Node l (Item key a) r) t' res
-- | /O(log n + log n')/. Pattern matching on @'Union' t@,
-- where @'Record' t'@ contains functions that return @res@ for /all/ items in @t@.
--
-- A special version of 'unioncaseX' with a tighter context.
unioncase :: Unioncasable t t' res => Union t -> Record t' -> res
unioncase = unioncaseX
class UnioncasableX (t :: BST *) (t' :: BST *) (res :: *) where
  -- | /O(log n + log n')/. Pattern matching on @'Union' t@,
  -- where @'Record' t'@ contains functions that return @res@ for /some/ items in @t@.
  --
  -- 'unioncase' with a looser context.
  unioncaseX :: Union t -> Record t' -> res
instance UnioncasableX Leaf t' res where
  unioncaseX !_ _ = undefined
  {-# INLINE unioncaseX #-}
instance (Searchable t' key (a -> res), UnioncasableX l t' res, UnioncasableX r t' res) => UnioncasableX (Node l (Item key a) r) t' res where
  unioncaseX (Top (Item x)) rc = case rc !? (Proxy :: Proxy key) of
    Just f -> f x
    Nothing -> error "Type.BST.BST.unioncaseX: corresponding function not found"
  unioncaseX (GoL l) rc = unioncaseX l rc
  unioncaseX (GoR r) rc = unioncaseX r rc
  {-# INLINABLE unioncaseX #-}

{- Inclusion -}

-- | @t@ includes @t'@; in other words, @t@ contains all items in @t'@.
class Includes (t :: BST *) (t' :: BST *)
instance Includes t Leaf
instance (Contains t key a, Includes t l, Includes t r) => Includes t (Node l (key |> a) r)
-- | @t@ is included by @t'@; in other words, @t'@ contains all items in @t@.
--
-- >IncludedBy t t' == Includes t' t
class Includes t' t => IncludedBy (t :: BST *) (t' :: BST *)
instance Includes t' t => IncludedBy t t'
-- | /O(n log n')/. A special version of 'metamorphose' with a tighter context guaranteeing the safety of the conversion.
shrink :: (Metamorphosable Record t t', Includes t t') => Record t -> Record t'
shrink = metamorphose
-- | /O(log n + log n')/. A special version of 'metamorphose' with a tighter context guaranteeing the safety of the conversion.
expand :: (Metamorphosable Union t t', IncludedBy t t') => Union t -> Union t'
expand = metamorphose

{- Balancing -}

class (Foldable t, Fromlistable (Tolist t)) => Balanceable (t :: BST *) where
  -- | /O(n log n)/ (no matter how unbalanced @t@ is). Balance an unbalanced 'BST'.
  --
  -- Doing a lot of insertions and deletions may cause an unbalanced BST.
  --
  -- If @t@ and @t'@ have the same items (just in different orders), @Balance t == Balance t'@,
  --
  -- Moreover, if @t@ and @as@ have the same items (just in different orders), @Balance t = Fromlist as@.
  type Balance t :: BST *
  -- | /O(n log n)/ (no matter how unbalanced @t@ is). Balance an unbalanced 'Record'.
  --
  -- If @rc@ and @rc'@ have the same items (just in different orders), @balance rc == balance rc'@,
  --
  -- Moreover, if @rc@ and @l@ have the same items (just in different orders), @balance rc = fromlist l@.
  balance :: Record t -> Record (Balance t)
instance (Foldable t, Fromlistable (Tolist t)) => Balanceable t where
  type Balance t = Fromlist (Tolist t)
  balance = fromlist . tolist
  {-# INLINE balance #-}

{- Metamorphosis -}

class Metamorphosable (f :: BST * -> *) (t :: BST *) (t' :: BST *) where
  -- | /O(n log n')/ when @f@ is 'Record' and /O(log n + log n')/ when @f@ is 'Union'.
  --
  -- Possibly unsafe conversion from @f t@ to @f t'@.
  metamorphose :: f t -> f t'
instance MetamorphosableR t t' => Metamorphosable Record t t' where
  metamorphose = metamorphoseR
  {-# INLINE metamorphose #-}
instance MetamorphosableU t t' => Metamorphosable Union t t' where
  metamorphose = metamorphoseU
  {-# INLINE metamorphose #-}
class MetamorphosableR t t' where
  metamorphoseR :: Record t -> Record t'
instance (Foldable t', MetamorphosableR' t t') => MetamorphosableR t t' where
  metamorphoseR = metamorphoseR' virtual
  {-# INLINE metamorphoseR #-}
class MetamorphosableR' (t :: BST *) (t' :: BST *) where
  metamorphoseR' :: Record t' -> Record t -> Record t'
instance MetamorphosableR' Leaf t where
  metamorphoseR' rc _ = rc
  {-# INLINE metamorphoseR' #-}
instance (Searchable t key a, MetamorphosableR' l t, MetamorphosableR' r t) => MetamorphosableR' (Node l (key |> a) r) t where
  metamorphoseR' rc (Triple l (Item x) r) = metamorphoseR' ((updateX (Proxy :: Proxy key) x) $ metamorphoseR' rc r) l
  {-# INLINABLE metamorphoseR' #-}
class MetamorphosableU (t :: BST *) (t' :: BST *) where
  metamorphoseU :: Union t -> Union t'
instance MetamorphosableU Leaf t where
  metamorphoseU _ = error "Type.BST.BST.metamorphoseU: bad luck!"
  {-# INLINE metamorphoseU #-}
instance (Searchable t key a, MetamorphosableU l t, MetamorphosableU r t) => MetamorphosableU (Node l (key |> a) r) t where
  metamorphoseU (Top (Item x)) = case injX (Proxy :: Proxy key) x of
    Nothing -> error "Type.BST.BST.metamorphoseU: bad luck!"
    Just u -> u
  metamorphoseU (GoL l) = metamorphoseU l
  metamorphoseU (GoR r) = metamorphoseU r
  {-# INLINABLE metamorphoseU #-}

{- Merging -}

class Mergeable (t :: BST *) (t' :: BST *) where
  -- | /O(n log n + n' log n')/. Merge two 'BST's.
  -- If @t@ and @t'@ has an @Item@ with the same key,
  -- only the one in @t@ will be added to the result.
  type Merge t t' :: BST *
  -- | /O(n log n + n' log n')/. Merge two 'Record's.
  merge :: Record t -> Record t' -> Record (Merge t t')
  -- | /O(n log n + n' log n')/. Merge two 'BST's in a more equal manner.
  -- If @t@ has @'Item' key a@ and @t'@ has @'Item' key b@,
  -- @'Item' key (a, b)@ will be added to the result.
  type MergeE t t' :: BST *
  -- | /O(n log n + n' log n')/. Merge two 'Record's in a more equal manner.
  mergeE :: Record t -> Record t' -> Record (MergeE t t')
instance (Foldable t, Foldable t', MergeableL (Tolist t) (Tolist t'), Fromlistable (MergeL (Tolist t) (Tolist t')), Fromlistable (MergeEL (Tolist t) (Tolist t'))) => Mergeable t t' where
  type Merge t t' = Fromlist (MergeL (Tolist t) (Tolist t'))
  merge rc rc' = fromlist $ mergeL (tolist rc) (tolist rc')
  {-# INLINE merge #-}
  type MergeE t t' = Fromlist (MergeEL (Tolist t) (Tolist t'))
  mergeE rc rc' = fromlist $ mergeEL (tolist rc) (tolist rc')
  {-# INLINE mergeE #-}

{- Insertion -}

class Insertable (t :: BST *) (key :: k) where
  -- | /O(log n)/. Insert @'Item' key a@ into the 'BST'.
  -- If @t@ already has an 'Item' with @key@, the 'Item' will be overwritten.
  -- The result may be unbalanced.
  type Insert t key (a :: *) :: BST *
  -- | /O(log n)/. Insert @a@ at @key@ into the 'Record'.
  -- The result may be unbalanced.
  insert :: proxy key -> a -> Record t -> Record (Insert t key a)
instance Insertable Leaf key where
  type Insert Leaf key a = Node Leaf (key |> a) Leaf
  insert _ x _ = Triple None ((Item :: With key) x) None
  {-# INLINE insert #-}
instance Insertable' (Compare key key') (Node l (key' |> b) r) key => Insertable (Node l (key' |> b) r) key where
  type Insert (Node l (key' |> b) r) key a = Insert' (Compare key key') (Node l (key' |> b) r) key a
  insert _ = insert' (Proxy :: Proxy '(Compare key key', key))
  {-# INLINE insert #-}
class Insertable' (o :: Ordering) (t :: BST *) (key :: k) where
  type Insert' o t key (a :: *) :: BST *
  insert' :: Proxy '(o, key) -> a -> Record t -> Record (Insert' o t key a)
instance Insertable' EQ (Node l (key |> _') r) key where
  type Insert' EQ (Node l (key |> _') r) key a = Node l (key |> a) r
  insert' _ x (Triple l _ r) = Triple l ((Item :: With key) x) r
  {-# INLINE insert' #-}
instance Insertable l key => Insertable' LT (Node l b r) key where
  type Insert' LT (Node l b r) key a = Node (Insert l key a) b r
  insert' _ x (Triple l y r) = Triple (insert (Proxy :: Proxy key) x l) y r
  {-# INLINABLE insert' #-}
instance Insertable r key => Insertable' GT (Node l b r) key where
  type Insert' GT (Node l b r) key a = Node l b (Insert r key a)
  insert' _ x (Triple l y r) = Triple l y (insert (Proxy :: Proxy key) x r)
  {-# INLINABLE insert' #-}

{- Deletion -}

class Deletable (t :: BST *) (key :: k) where
  -- | /O(log n)/. Delete an 'Item' at @key@ from the 'BST'.
  -- If the BST does not have any item at @key@, the original BST will be returned.
  -- The result may be unbalanced.
  type Delete t key :: BST *
  -- | /O(log n)/. Delete an 'Item' at @key@ from the 'Record'.
  delete :: proxy key -> Record t -> Record (Delete t key)
instance Deletable Leaf key where
  type Delete Leaf key = Leaf
  delete _ rc = rc
  {-# INLINE delete #-}
instance Deletable' (Compare key key') (Node l (key' |> a) r) key => Deletable (Node l (key' |> a) r) key where
  type Delete (Node l (key' |> a) r) key = Delete' (Compare key key') (Node l (key' |> a) r) key
  delete _ = delete' (Proxy :: Proxy '(Compare key key', key))
  {-# INLINE delete #-}
class Deletable' (o :: Ordering) (t :: BST *) (key :: k) where
  type Delete' o t key :: BST *
  delete' :: Proxy '(o, key) -> Record t -> Record (Delete' o t key)
instance Deletable' EQ (Node Leaf (key |> _') r) key where
  type Delete' EQ (Node Leaf (key |> _') r) key = r
  delete' _ (Triple _ _ r) = r
  {-# INLINE delete' #-}
instance Deletable' EQ (Node (Node l a r) (key |> _') Leaf) key where
  type Delete' EQ (Node (Node l a r) (key |> _') Leaf) key = Node l a r
  delete' _ (Triple l _ _) = l
  {-# INLINE delete' #-}
instance Maxable (Node l a r) => Deletable' EQ (Node (Node l a r) (key |> _') (Node l' b r')) key where
  type Delete' EQ (Node (Node l a r) (key |> _') (Node l' b r')) key = Node (Deletemax (Node l a r)) (Findmax (Node l a r)) (Node l' b r')
  delete' _ (Triple l _ r) = let (y, rc) = splitmax l in Triple rc y r
  {-# INLINE delete' #-}
instance Deletable l key => Deletable' LT (Node l a r) key where
  type Delete' LT (Node l a r) key = Node (Delete l key) a r
  delete' _ (Triple l x r) = Triple (delete (Proxy :: Proxy key) l) x r
  {-# INLINE delete' #-}
instance Deletable r key => Deletable' GT (Node l a r) key where
  type Delete' GT (Node l a r) key = Node l a (Delete r key)
  delete' _ (Triple l x r) = Triple l x (delete (Proxy :: Proxy key) r)
  {-# INLINE delete' #-}

{- Min and Max -}

class Minnable (t :: BST *) where
  -- | /O(log n)/. Delete the 'Item' at the minimum key from the 'BST'.
  -- The result may be unbalanced.
  type Deletemin t :: BST *
  -- | /O(log n)/. Get the value type of the 'Item' at the minimum key in the 'BST'.
  type Findmin t :: *
  -- | /O(log n)/. Split the 'Record' into (the value of) the 'Item' at the minimum key and the rest.
  splitmin :: Record t -> (Findmin t, Record (Deletemin t))
-- | /O(log n)/. Get the value of the 'Item' at the minimum key in the 'Record'.
--
-- >findmin = fst . splitmin
findmin :: Minnable t => Record t -> Findmin t
findmin = fst . splitmin
{-# INLINE findmin #-}
-- | /O(log n)/. Delete the 'Item' at the minimum key from the 'Record'.
--
-- >deletemin = snd . splitmin
deletemin :: Minnable t => Record t ->Record (Deletemin t)
deletemin = snd . splitmin
{-# INLINE deletemin #-}
instance Minnable (Node Leaf a r) where
  type Deletemin (Node Leaf a r) = r
  type Findmin (Node Leaf a r) = a
  splitmin (Triple _ x r) = (x, r)
  {-# INLINE splitmin #-}
instance Minnable (Node l' b r') => Minnable (Node (Node l' b r') a r) where
  type Deletemin (Node (Node l' b r') a r) = Node (Deletemin (Node l' b r')) a r
  type Findmin (Node (Node l' b r') a r) = Findmin (Node l' b r')
  splitmin (Triple l x r) = let (y, rc) = splitmin l in (y, Triple rc x r)
  {-# INLINABLE splitmin #-}
class Maxable (t :: BST *) where
  -- | /O(log n)/. Delete the 'Item' at the maximum key from the 'BST'.
  -- The result may be unbalanced.
  type Deletemax t :: BST *
  type Findmax t :: *
  -- | /O(log n)/. Get the value type of the 'Item' at the maximum key in the 'BST'.
  splitmax :: Record t -> (Findmax t, Record (Deletemax t))
-- | /O(log n)/. Get the value of the 'Item' at the maximum key in the 'Record'.
--
-- >findmin = fst . splitmin
findmax :: Maxable t => Record t -> Findmax t
findmax = fst . splitmax
{-# INLINE findmax #-}
-- | /O(log n)/. Delete the 'Item' at the maximum key from the 'Record'.
--
-- >deletemin = snd . splitmin
deletemax :: Maxable t => Record t ->Record (Deletemax t)
deletemax = snd . splitmax
{-# INLINE deletemax #-}
instance Maxable (Node l a Leaf) where
  type Deletemax (Node l a Leaf) = l
  type Findmax (Node l a Leaf) = a
  splitmax (Triple l x _) = (x, l)
  {-# INLINE splitmax #-}
instance Maxable (Node l' b r') => Maxable (Node l a (Node l' b r')) where
  type Deletemax (Node l a (Node l' b r')) = Node l a (Deletemax (Node l' b r'))
  type Findmax (Node l a (Node l' b r')) = Findmax (Node l' b r')
  splitmax (Triple l x r) = let (y, rc) = splitmax r in (y, Triple l x rc)
  {-# INLINABLE splitmax #-}
