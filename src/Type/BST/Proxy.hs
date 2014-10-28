{-# LANGUAGE Safe,
    PolyKinds #-}

module Type.BST.Proxy (
    Proxy(..)
  ) where

data Proxy (a :: k) = Proxy
