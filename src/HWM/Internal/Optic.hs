
module HWM.Internal.Optic (
(^.), (%~), (.~), to,
) where

import Data.Functor.Contravariant
import Data.Functor.Const
import Data.Functor.Identity
import Data.Coerce (coerce)

view :: ((a -> Const a a) -> b -> Const a b) -> b -> a
-- view o = getConst . o Const
view o = coerce (o Const)
{-# INLINE view #-}

(^.) :: b -> ((a -> Const a a) -> b -> Const a b) -> a
x ^. o = view o x
{-# INLINE (^.) #-}

to :: (Contravariant m)=> (a -> b) -> (b -> m b) -> a -> m a
to f g = contramap f . g . f
{-# INLINE to #-}


(%~) :: ((a -> Identity b) -> c -> Identity d) -> (a -> b) -> c -> d
-- o %~ f = runIdentity . o (Identity . f)
(%~) = coerce
{-# INLINE (%~) #-}

(.~) :: ((a -> Identity b) -> c -> Identity d) -> b -> c -> d
o .~ x = o %~ \_-> x
{-# INLINE (.~) #-}
