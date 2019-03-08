module Some where

import Control.Applicative (liftA2)
import Data.Map (Map)
import qualified Data.Map as M

data InterMed k v = BothEnd
                  | ValEnd (Maybe k) (InterMed k v)
                  | KeyEnd (Maybe v) (InterMed k v)
                  | Next (Maybe (k, v)) (InterMed k v)
                  deriving (Show, Eq)

foldIM :: (Ord k) =>
  ((t, a1 -> t -> t, a2 -> t -> t, t2 -> t -> t), (a1, k -> a1), (a2, v -> a2), (t2, (k, v) -> t2))
  -> InterMed k v -> t
foldIM phi@((e,v,k,n),(c1,f1),(c2,f2),(c3,f3)) m = case m of
  BothEnd -> e
  ValEnd a x -> v (foldMk  phi a) (foldIM phi x)
  KeyEnd a x -> k (foldMv  phi a) (foldIM phi x)
  Next a x -> n (foldMkv phi a) (foldIM phi x)

foldMk phi@((e,v,k,n),(c1,f1),(c2,f2),(c3,f3)) m = case m of
  Nothing -> c1
  Just a  -> f1 a

foldMv phi@((e,v,k,n),(c1,f1),(c2,f2),(c3,f3)) m = case m of
  Nothing -> c2
  Just a  -> f2 a

foldMkv phi@((e,v,k,n),(c1,f1),(c2,f2),(c3,f3)) m = case m of
  Nothing -> c3
  Just a  -> f3 a

data MaybeIM k v a = BE
             | VE (Maybe k) a
             | KE (Maybe v) a
             | NT (Maybe (k, v)) a
             deriving (Show, Eq)

unfoldIM psis@(psi, psi1, psi2, psi3) x = case psi x of
  BE       -> BothEnd
  VE mk  m -> ValEnd (unfoldMk  psis mk)  (unfoldIM psis m)
  KE mv  m -> KeyEnd (unfoldMv  psis mv)  (unfoldIM psis m)
  NT mkv m -> Next (unfoldMkv psis mkv) (unfoldIM psis m)

unfoldMk  psis@(psi, psi1, psi2, psi3) x = psi1 x
unfoldMv  psis@(psi, psi1, psi2, psi3) x = psi2 x
unfoldMkv psis@(psi, psi1, psi2, psi3) x = psi3 x

psi :: ([Maybe k], [Maybe v]) -> MaybeIM k v ([Maybe k], [Maybe v])
psi ([],[])      = BE
psi (k:ks, [])   = VE k (ks, [])
psi ([], v:vs)   = KE v ([], vs)
psi (k:ks, v:vs) = NT (liftA2 (,) k v) (ks, vs)

psis = (psi, id, id, id)
{-
phi = (e, v, k, n)
  where
    e = Just M.empty
    v Nothing _ = Nothing
    v (Just _) z = z
    k Nothing _ = Nothing
    k (Just _) z = z
    n Nothing _ = Nothing
    n (Just (k, v)) z = undefined -- Just (M.insert k v z)
-}
-- phis = (phi, id, id, id)

-- some = foldIM phis . unfoldIM psis

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


