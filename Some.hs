module Some where

import Data.Map (Map)
import qualified Data.Map as M

some :: Ord k => ([Maybe k], [Maybe v]) -> Maybe (Map k v)
some = hylo phi z psi
  where
    z = Just M.empty
    phi (Just Nothing, _) _ = Nothing
    phi (_, Just Nothing) _ = Nothing
    phi _ Nothing = Nothing
    phi (Nothing, _) _     = z
    phi (_, Nothing) _     = z
    phi (Just (Just k), Just (Just v)) (Just m) = Just (M.insert k v m)
    psi ([], [])   = Nothing
    psi (k:ks, []) = Just ((Just k, Nothing), (ks, []))
    psi ([], v:vs) = Just ((Nothing, Just v), ([], vs))
    psi (k:ks,v:vs) = Just ((Just k, Just v), (ks,vs))
    
hylo :: (b -> c -> c) -> c -> (a -> Maybe (b, a)) -> a -> c
hylo phi z psi x = case psi x of
  Nothing      -> z
  Just (y, x') -> phi y (hylo phi z psi x')

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

