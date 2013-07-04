> {-# LANGUAGE DefaultSignatures #-}

> module Control.Parallel.Data where

> import Prelude hiding (map, filter)

> import Data.Monoid
> import Control.Monad
> import Control.Comonad
> import Data.Distributive

Cartesian-closed helpers

> pair :: (a -> b) -> (a -> c) -> (a -> (b, c))
> pair f g = \x -> (f x, g x)

> class (Map t, Promote t, Zip t, 
>        Reduce t, Filter t,
>        Gather t, GatherMask t,
>        Scatter t, ScatterOver t) => DataPar t where

> class Map t where
>     map :: (a -> b) -> t a -> t b
>     default map :: Functor t => (a -> b) -> t a -> t b
>     map = fmap

> class Promote t where
>     promote :: a -> t a
>     default promote :: Monad t => a -> t a
>     promote = return 

> class Zip t where
>     zip :: (t a, t b) -> t (a, b)

> class Reduce t where
>     reduce :: Monoid x => t (x, a) -> (x, t a) 

>     reduceMap :: Monoid x => (a -> (x, b)) -> t a -> (x, t b)
>     default reduceMap :: (Map t, Monoid x) => (a -> (x, b)) -> t a -> (x, t b)
>     reduceMap f = reduce . (map f)

> class Filter t where
>     filter :: t (Maybe a) -> Maybe (t a)

>     filterMap :: (a -> Maybe b) -> t a -> Maybe (t b)
>     default filterMap :: Map t => (a -> Maybe b) -> t a -> Maybe (t b)
>     filterMap f = filter . (map f)

> class Gather t where
>     gather :: t a -> t (t a)
>     default gather :: (Comonad t, Functor t) => t a -> (t (t a))
>     gather = extend id

>     current :: t a -> a
>     default current :: Comonad t => t a -> a
>     current = extract
>              
>     gatherMap :: (t a -> b) -> t a -> t b
>     default gatherMap :: (Map t) => (t a -> b) -> t a -> t b
>     gatherMap f = (map f) . gather

> class GatherMask t where
>     gatherMapMask :: (t a -> a) -> t a -> t a
>     default gatherMapMask :: (Comonad t, Functor t) => (t a -> a) -> t a -> t a
>     gatherMapMask = extend

> class Scatter t where
>     scatter :: t (t a) -> t a
>     default scatter :: Monad t => t (t a) -> t a
>     scatter = join

>     local :: a -> t a
>     default local :: Monad t => a -> t a
>     local = return

>     mapScatter :: (a -> t b) -> t a -> t b
>     default mapScatter :: Map t => (a -> t b) -> t a -> t b
>     mapScatter f = scatter . (map f)


> class ScatterOver t where
>     scatterOver :: t (t a) -> t a

>     mapScatterOver :: (a -> t b) -> t a -> t b
>     default mapScatterOver :: Map t => (a -> t b) -> t a -> t b
>     mapScatterOver f = scatterOver . (map f)

