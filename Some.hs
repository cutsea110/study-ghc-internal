module Some where

import Control.Applicative (liftA2)
import Data.Map (Map)
import qualified Data.Map as M

data InterMed k v = End
                  | Val (Maybe k) (InterMed k v)
                  | Key (Maybe v) (InterMed k v)
                  | Nxt (Maybe (k, v)) (InterMed k v)
                  deriving (Show, Eq)

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


some :: (Ord k) => ([Maybe k], [Maybe v]) -> Maybe (M.Map k v)
some = undefined

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


