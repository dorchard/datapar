> {-# LANGUAGE FlexibleInstances #-}

 {-# LANGUAGE DefaultSignatures #-}

> import Control.Parallel.Data
> import Prelude hiding (map)
> import qualified Prelude as Pre

> import Control.Comonad

> import Data.Monoid
> import Data.Complex


> data Vector a = Vector {vectorData :: [a], cursor :: Int} deriving Show

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


> instance Monoid (Complex Double) where
>     mempty = 0 :+ 0
>     mappend = (+)

> st :: a -> Vector b -> Vector (a, b)
> st a bs = map (\b -> (a, b)) bs

> dft :: Vector Double -> Vector (Complex Double)
> dft x = map (\x' -> fst . reduce . (map (\x'' -> (args (cursor x') (cursor x'') (current x''), current x''))) . gather $ x') . gather $ x
>            where         
>                args k i x = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral i) * (0 :+ 1)) / n)
>                n = fromIntegral (length . vectorData $ x)
> 
> dft x = map (\x' -> fst . reduce . (map (\x'' -> (args (fst $ current x'') (cursor x'') (snd $ current x''), current x''))) . gather $ st (cursor x') x') . gather $ x
>            where         
>                args k i x = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral i) * (0 :+ 1)) / n)
>                n = fromIntegral (length . vectorData $ x)





> dft' :: Vector Double -> Vector (Complex Double)
> dft' x = map (\x' -> fst . reduce . (map (\x'' -> (args (cursor x') (cursor x'') (current x''), current x''))) $ x') . gather . gather $ x
>             where         
>                 args k j x = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral j) * (0 :+ 1)) / n)
>                 n = fromIntegral (length . vectorData $ x)


 dft'' :: Vector Double -> Vector (Complex Double)

dft'' :: Vector Double -> Vector (Complex Double)
 dft'' x = map (fst . reduce)(map (\x'' -> ((current x'') (cursor x''), current x'')))) . gather . (map (\x' -> args (cursor x') (current x'))) . gather $ x
             where         
                 args k x j = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral j) * (0 :+ 1)) / n)
                 n = fromIntegral (length . vectorData $ x)





   

 dft x = map (\x' -> fst . reduce . (map (\x'' -> (args (fst $ current x'') (cursor x'') (snd $ current x''), current x''))) $ st (cursor x') x') . gather . gather $ x
            where         
                args k j x = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral j) * (0 :+ 1)) / n)
                n = fromIntegral (length . vectorData $ x)



 dft x = map (\x' -> fst . reduce . (map (\x'' -> (args (cursor x') (cursor x'') (current x''), current x''))) $ x') . gather . gather $ x
            where         
                args k j x = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral j) * (0 :+ 1)) / n)
                n = fromIntegral (length . vectorData $ x)

 dft x = map (\x' -> fst . reduce . (map (\x'' -> (args (fst $ current x'') (cursor x'') (snd $ current x''), undefined))) $ st (ursor x') x') . gather . gather $ x
            where         
                args k j x = (x :+ 0) * exp ((-2 * pi * (fromIntegral k) * (fromIntegral j) * (0 :+ 1)) / n)
                n = fromIntegral (length . vectorData $ x)

