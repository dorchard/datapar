> {-# LANGUAGE FlexibleInstances #-}

> module Vector where

> import Control.Parallel.Data
> import Control.Comonad
> import Prelude hiding (map, zip)
> import qualified Prelude as Pre
> import Data.Monoid

> data Vector a = Vector {vectorData :: [a], cursor :: Int} deriving (Eq, Show)


> index :: Monoid a => Vector a -> Int -> a
> index (Vector xs _) n = if (n >= (length xs))
>                         then mempty
>                         else xs !! n


> instance Comonad Vector where
>     extract (Vector x n) = x !! n
>     extend k (Vector x n) = Vector (Pre.map (\c -> k (Vector x c)) [0..((length x) - 1)]) n

> instance Gather Vector where

> instance Reduce Vector where
>     reduce (Vector x n) = let (r, x') = go x
>                           in (r, Vector x' n)
>                                where go [] = (mempty, [])
>                                      go ((r, x):xs) = let (r', xs') = go xs
>                                                       in (r `mappend` r', x : xs')

> instance Functor Vector where
>     fmap f (Vector x n) = Vector (Pre.map f x) n

> instance Map Vector where

> instance Zip Vector where
>     zip (a@(Vector x n), b@(Vector y n')) | n < n'  = zip (shiftR a, b)
>                                           | n > n'  = zip (a, shiftR b)
>                                           | n == n' = Vector (Pre.zip x y) n
>                                           where shiftR (Vector x n) = Vector (x ++ [undefined]) (n + 1) 

> instance Promote Vector where
>     promote x = Vector (repeat x) 0



Test data

> foox :: Vector Double
> foox = Vector [1,2,3,0,1,5,2,3,4,1] 0
