{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module GenericTraverse where

import Generics.SOP
import Data.Typeable
import Data.Functor.Identity
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

genericTraversal :: (Typeable a, Generic s, All2 Typeable (Code s)) => Traversal' s a
genericTraversal f x = to <$> gTravS f (from x)

gTravS :: (Typeable a, Applicative f, All2 Typeable xss) => (a -> f a) -> SOP I xss -> f (SOP I xss)
gTravS f (SOP xss) =
  SOP <$> hsequence' (hcmap (Proxy :: Proxy (All Typeable)) (gTravP f) xss)

type family Head (xs :: [*]) where
   Head (x ': xs) = x

overTypeable :: forall a b f. (Typeable a, Typeable b, Applicative f) => (a -> f a) -> b -> f b
overTypeable f x =
  case eqT :: Maybe (a :~: b) of
    Nothing -> pure x
    Just Refl -> f x

gTravP :: forall a f xs. (Typeable a, Applicative f, All Typeable xs) => (a -> f a) -> NP I xs -> (f :.: NP I) xs
gTravP f s = Comp (hsequence' (hcmap (Proxy :: Proxy Typeable) f' s))
  where f' :: forall b. Typeable b => I b -> (f :.: I) b
        f' (I b) = Comp (I <$> overTypeable f b)

over :: Traversal s t a b -> (a -> b) -> s -> t
over trav f = runIdentity . trav (Identity . f)
