> {-# LANGUAGE FlexibleInstances #-}

> import Control.Parallel.Data
> import Control.Comonad
> import Prelude hiding (map, zip)
> import qualified Prelude as Pre

> import Vector 

> import Data.Monoid
> import Data.Maybe

> data SparseVector a = SV { svectorData :: [(Int, a)], scursor :: Int } deriving (Eq, Show)

> instance Functor SparseVector where
>     fmap f (SV xs n) = SV (Pre.map (\(i, x) -> (i, f x)) xs) n

> instance Comonad SparseVector where
>     extract (SV xs n) = fromJust $ lookup n xs
>     extend k (SV xs n) = SV (Pre.map (\(i, _) -> (i, k (SV xs i))) xs) n

> instance Map SparseVector where
> instance Gather SparseVector where

> instance Reduce SparseVector where
>     reduce (SV xs n) = let (r, xs') = go xs
>                        in (r, SV xs' n)
>                             where go [] = (mempty, [])
>                                   go ((i, (r, x)):xs) = let (r', xs') = go xs
>                                                         in (r `mappend` r', (i, x) : xs')

 instance Zip SparseVector where
     zip (a@(SV x n), b@(SV y n')) | n < n'  = zip (shiftR a, b)
                                   | n > n'  = zip (a, shiftR b)
                                   | n == n' = SV (Pre.zip x y) n
                                   where shiftR (Vector x n) = Vector (x ++ [undefined]) (n + 1) 

> instance Promote SparseVector where
>     promote x = SparseVector (Pre.zip (repeat x) [0..]) 0


 spareToDense :: Monoid a => SparseVector a -> Vector a
 spareToDense (SV xs n) = 

> type SparseMatrix a = Vector (SpareVector a)


>     