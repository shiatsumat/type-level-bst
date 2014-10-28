{-# LANGUAGE Trustworthy,
    TypeOperators, ScopedTypeVariables,
    PolyKinds, DataKinds,
    FlexibleInstances, UndecidableInstances #-}

module Type.BST.Showtype (
    -- * Showtype
    Showtype(showtype, showtypesPrec)
  ) where

import GHC.TypeLits
import Type.BST.Proxy

-- | Conversion of types to readable 'String's. Analogous to 'Show'.
class Showtype (a :: k) where
  {-# MINIMAL showtype | showtypesPrec #-}
  -- | Convert a type @a@ to a readable 'String'. Analogous to 'show' in 'Show'.
  showtype :: proxy a -> String
  -- | Convert a type @a@ to a readable 'String' with additional arguments. Analogous to 'showsPrec' in 'Show'.
  showtypesPrec :: Int -> proxy a -> String -> String
  showtype p = showtypesPrec 0 p ""
  showtypesPrec _ p s = showtype p ++ s
instance Showtype False where
  showtype _ = "False"
instance Showtype True where
  showtype _ = "True"
instance Showtype LT where
  showtype _ = "LT"
instance Showtype EQ where
  showtype _ = "EQ"
instance Showtype GT where
  showtype _ = "GT"
instance KnownNat n => Showtype (n :: Nat) where
  showtype = show . natVal
instance KnownSymbol s => Showtype (s :: Symbol) where
  showtype = show . symbolVal
instance Showtype Nothing where
  showtype _ = "Nothing"
instance Showtype a => Showtype (Just a) where
  showtypesPrec p _ = showParen (p > 10) $
    showString "Just " .
    showtypesPrec 11 (Proxy :: Proxy a)
instance Showtype a => Showtype (Left a) where
  showtypesPrec p _ = showParen (p > 10) $
    showString "Left " .
    showtypesPrec 11 (Proxy :: Proxy a)
instance Showtype a => Showtype (Right a) where
  showtypesPrec p _ = showParen (p > 10) $
    showString "Right " .
    showtypesPrec 11 (Proxy :: Proxy a)
instance Showtype '[] where
  showtype _ = "[]"
instance (Showtype a, Showlisttype as) => Showtype (a ': as :: [k]) where
  showtype _ = "[" ++ showtype (Proxy :: Proxy a) ++ showlisttype (Proxy :: Proxy as)
class Showlisttype (as :: [k]) where
  showlisttype :: Proxy as -> String
instance Showlisttype '[] where
  showlisttype _ = "]"
instance (Showtype a, Showlisttype as) => Showlisttype (a ': as) where
  showlisttype _ = "," ++ showtype (Proxy :: Proxy a) ++ showlisttype (Proxy :: Proxy as)
showtuple :: [String] -> String
showtuple ss = "(" ++ foldr1 (\s t -> s ++ "," ++ t) ss ++ ")"
instance (Showtype a, Showtype b) => Showtype (a,b) where
  showtype _ = showtuple [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b)]
instance (Showtype a, Showtype b, Showtype c) => Showtype (a,b,c) where
  showtype _ = showtuple [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c)]
instance (Showtype a, Showtype b, Showtype c, Showtype d) => Showtype (a,b,c,d) where
  showtype _ = showtuple [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e) => Showtype (a,b,c,d,e) where
  showtype _ = showtuple [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e, Showtype f) => Showtype (a,b,c,d,e,f) where
  showtype _ = showtuple [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e),
    showtype (Proxy :: Proxy f)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e, Showtype f, Showtype g) => Showtype (a,b,c,d,e,f,g) where
  showtype _ = showtuple [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e),
    showtype (Proxy :: Proxy f),
    showtype (Proxy :: Proxy g)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e, Showtype f, Showtype g, Showtype h) => Showtype (a,b,c,d,e,f,g,h) where
  showtype _ = showtuple [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e),
    showtype (Proxy :: Proxy f),
    showtype (Proxy :: Proxy g),
    showtype (Proxy :: Proxy h)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e, Showtype f, Showtype g, Showtype h, Showtype i) => Showtype (a,b,c,d,e,f,g,h,i) where
  showtype _ = showtuple [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e),
    showtype (Proxy :: Proxy f),
    showtype (Proxy :: Proxy g),
    showtype (Proxy :: Proxy h),
    showtype (Proxy :: Proxy i)]
instance (Showtype a, Showtype b, Showtype c, Showtype d, Showtype e, Showtype f, Showtype g, Showtype h, Showtype i, Showtype j) => Showtype (a,b,c,d,e,f,g,h,i,j) where
  showtype _ = showtuple [
    showtype (Proxy :: Proxy a),
    showtype (Proxy :: Proxy b),
    showtype (Proxy :: Proxy c),
    showtype (Proxy :: Proxy d),
    showtype (Proxy :: Proxy e),
    showtype (Proxy :: Proxy f),
    showtype (Proxy :: Proxy g),
    showtype (Proxy :: Proxy h),
    showtype (Proxy :: Proxy i),
    showtype (Proxy :: Proxy j)]
