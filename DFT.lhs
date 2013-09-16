> {-# LANGUAGE FlexibleInstances #-}

 {-# LANGUAGE DefaultSignatures #-}

> import Control.Parallel.Data
> import Prelude hiding (map, zip)

> import Data.Monoid
> import Data.Complex

> import Vector

> instance Monoid (Complex Double) where
>     mempty = 0 :+ 0
>     mappend = (+)

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

