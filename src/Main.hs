{-# LANGUAGE TypeOperators, PolyKinds, DataKinds #-}

module Main (module Main) where

import Type.BST

data Proxy k = Proxy
p0 = Proxy :: Proxy 0
p1 = Proxy :: Proxy 1
p2 = Proxy :: Proxy 2
p3 = Proxy :: Proxy 3
p4 = Proxy :: Proxy 4
p5 = Proxy :: Proxy 5

type T = Fromlist [0 |> Bool, 4 |> String, 2 |> Int, 3 |> Double, 1 |> Char]
rct :: Record T
rct = fromlist $ p3 |> 3 .:. p0 |> True .:. p2 |> 10 .:. p4 |> "wow" .:. p1 |> 'a' .:. Nil
ut :: Union T
ut = inj p3 10
type T' = Fromlist [0 |> Bool, 1 |> Char, 2 |> Int, 3 |> Double]
type T'' = Fromlist [0 |> Bool, 1 |> Char, 2 |> Int, 3 |> Double, 4 |> String, 5 |> Int]
type U = Fromlist [0 |> Bool, 1 |> Double, 2 |> String, 3 |> Double, 4 |> String, 5 |> Int]
rcu :: Record U
rcu = fromlist $ p2 |> "hi" .:. p5 |> 6 .:. p0 |> False .:. p3 |> 3 .:. p1 |> 3.14 .:. p4 |> "yeah" .:. Nil

main = print 0
