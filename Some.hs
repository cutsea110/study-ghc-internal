module Some where

import Control.Applicative (liftA2)
import Data.Map (Map)
import qualified Data.Map as M

data InterMed k v = End
                  | Val (Maybe k) (InterMed k v)
                  | Key (Maybe v) (InterMed k v)
                  | Nxt (Maybe (k, v)) (InterMed k v)
                  deriving (Show, Eq)

foldIM :: (Ord k) =>
  ((t, a1 -> t -> t, a2 -> t -> t, t2 -> t -> t), (a1, k -> a1), (a2, v -> a2), (t2, (k, v) -> t2))
  -> InterMed k v -> t
foldIM phi@((e,v,k,n),(c1,f1),(c2,f2),(c3,f3)) m = case m of
  End -> e
  Val a x -> v (foldMk  phi a) (foldIM phi x)
  Key a x -> k (foldMv  phi a) (foldIM phi x)
  Nxt a x -> n (foldMkv phi a) (foldIM phi x)

foldMk phi@((e,v,k,n),(c1,f1),(c2,f2),(c3,f3)) m = case m of
  Nothing -> c1
  Just a  -> f1 a

foldMv phi@((e,v,k,n),(c1,f1),(c2,f2),(c3,f3)) m = case m of
  Nothing -> c2
  Just a  -> f2 a

foldMkv phi@((e,v,k,n),(c1,f1),(c2,f2),(c3,f3)) m = case m of
  Nothing -> c3
  Just a  -> f3 a

data X k v a = E
             | V (Maybe k) a
             | K (Maybe v) a
             | N (Maybe (k, v)) a
             deriving (Show, Eq)

unfoldIM psis@(psi, psi1, psi2, psi3) x = case psi x of
  E       -> End
  V mk  m -> Val (unfoldMk  psis mk)  (unfoldIM psis m)
  K mv  m -> Key (unfoldMv  psis mv)  (unfoldIM psis m)
  N mkv m -> Nxt (unfoldMkv psis mkv) (unfoldIM psis m)

unfoldMk  psis@(psi, psi1, psi2, psi3) x = psi1 x
unfoldMv  psis@(psi, psi1, psi2, psi3) x = psi2 x
unfoldMkv psis@(psi, psi1, psi2, psi3) x = psi3 x

psi :: ([Maybe k], [Maybe v]) -> X k v ([Maybe k], [Maybe v])
psi ([],[])      = E
psi (k:ks, [])   = V k (ks, [])
psi ([], v:vs)   = K v ([], vs)
psi (k:ks, v:vs) = N (liftA2 (,) k v) (ks, vs)

{-
some :: (Ord k) => ([Maybe k], [Maybe v]) -> Maybe (M.Map k v)
some = foldIM phi . unfoldIM psi
  where
    phi = undefined
    psi = undefined
-}

{-
pairWith f (Just x) (Just y) = Just (f x y)
pairWith _ _        _        = Nothing

pair = pairWith (,)
left = pairWith const
cross f g (x, y) = (f x, g y)

some :: [Maybe a] -> [Maybe b] -> Maybe [(a, b)]
some xs ys = sequence ps `left` sequence rs
  where
    dummy = repeat (Just undefined)
    (ps, rs) = go xs ys []
    go (x:xs) (y:ys) ps = cross (pair x y:) id $ go xs ys ps
    go []     ys     ps = (ps, zipWith pair dummy ys)
    go xs     []     ps = (ps, zipWith pair xs dummy)
-}
keys = [Just 1, Just 2, Just 3]
vals = [Just 'a', Just 'b', Just 'c']

keys0 = [Just 1, Just 2, Just 3]
vals0 = [Just 'a', Just 'b', Nothing]

keys1 = [Just 1, Nothing, Just 3]
vals1 = [Just 'a', Just 'b', Just 'c']

keys2 = [Just 1, Just 2, Just 3, Just 4]
vals2 = [Just 'a', Just 'b', Just 'c']

keys2' = [Just 1, Just 2, Just 3]
vals2' = [Just 'a', Just 'b', Just 'c', Just 'd']

keys3 = [Just 1, Just 2, Just 3, Nothing]
vals3 = [Just 'a', Just 'b', Just 'c']

keys4 = [Just 1, Just 2, Just 3]
vals4 = [Just 'a', Just 'b', Just 'c', Nothing]

keys5 = [Just 1, Just 2, Just 3]
vals5 = [Just 'a', Just 'b', Just 'c', Just 'd', Just 'e']

keys6 = [Just 1, Just 2, Just 3]
vals6 = [Just 'a', Just 'b', Just 'c', Nothing, Just 'e']


