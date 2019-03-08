module Some where

import Control.Applicative (liftA2)
import Data.Map (Map)
import qualified Data.Map as M

data InterMed k v = BothEnd
                  | ValEnd (Maybe k) (InterMed k v)
                  | KeyEnd (Maybe v) (InterMed k v)
                  | Next (Maybe (k, v)) (InterMed k v)
                  deriving (Show, Eq)

phi :: (Ord k) => M.Map k v -> InterMed k v -> Maybe (M.Map k v)
phi z BothEnd = Just z
phi _ (ValEnd Nothing _)  = Nothing
phi z (ValEnd (Just _) r) = phi z r
phi _ (KeyEnd Nothing _)  = Nothing
phi z (KeyEnd (Just _) r) = phi z r
phi _ (Next Nothing _)    = Nothing
phi z (Next (Just (k, v)) r) = phi (M.insert k v z) r

psi :: ([Maybe k], [Maybe v]) -> InterMed k v
psi ([], []) = BothEnd
psi (mk:mks, []) = ValEnd mk (psi (mks, []))
psi ([], mv:mvs) = KeyEnd mv (psi ([], mvs))
psi (mk:mks, mv:mvs) = Next (liftA2 (,) mk mv) (psi (mks, mvs))

some :: (Ord k) => ([Maybe k], [Maybe v]) -> Maybe (M.Map k v)
some = phi z . psi
  where z = M.empty

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


