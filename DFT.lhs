> {-# LANGUAGE FlexibleInstances #-}

 {-# LANGUAGE DefaultSignatures #-}

> import Control.Parallel.Data
> import Prelude hiding (map, zip)
> import qualified Prelude as Pre

> import Control.Comonad

> import Data.Monoid
> import Data.Complex

> data Vector a = Vector {vectorData :: [a], cursor :: Int} deriving (Eq, Show)

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

> instance Monoid (Complex Double) where
>     mempty = 0 :+ 0
>     mappend = (+)

Test data

> foox :: Vector Double
> foox = Vector [1,2,3,0,1,5,2,3,4,1] 0

Examples with various transformations applied

> dft :: Vector Double -> Vector (Complex Double)
> dft x = map (\x' -> fst . reduce . (map (\x'' -> (args (cursor x') (cursor x'') (current x''), current x''))) . gather $ x') . gather $ x
>            where         
>                args k i x = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral i) * (0 :+ 1)) / n)
>                n = fromIntegral (length . vectorData $ x)
> 

> dft' x = map (\x' -> fst . reduce . (map (\x'' -> (args (fst $ current x'') (cursor x'') (snd $ current x''), current x''))) . gather $ (zip (promote (cursor x'), x'))) . gather $ x
>            where         
>                args k i x = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral i) * (0 :+ 1)) / n)
>                n = fromIntegral (length . vectorData $ x)


> dft'' :: Vector Double -> Vector (Complex Double)
> dft'' x = map (\x' -> fst . reduce . (map (\x'' -> (args (cursor x') (cursor x'') (current x''), current x''))) $ x') . gather . gather $ x
>             where         
>                 args k j x = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral j) * (0 :+ 1)) / n)
>                 n = fromIntegral (length . vectorData $ x)


Point-free version

> dft3 x = map (fst . reduce . (map ((args . (current `pair` cursor)) `pair` current)) . gather . zip . ((promote . cursor) `pair` id)) . gather $ x
>            where         
>                args ((k, x), i) = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral i) * (0 :+ 1)) / n)
>                n = fromIntegral (length . vectorData $ x)

