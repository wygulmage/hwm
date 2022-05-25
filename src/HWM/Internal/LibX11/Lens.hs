
module HWM.Internal.LibX11.Lens where


import HWM.Internal.Optic
import Data.Function ((&))
import qualified Foreign.C.Types as C
import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Extras as X11
import qualified Graphics.X11.Xlib.Types as X11

data Dimensions =
    Dimensions !X11.Dimension {-^ width -} !X11.Dimension {-^ height -}

class HasDimensions a where
{-^ A class for types that have width and height. -}
    dimensions ::
        (Functor m)=> (Dimensions -> m Dimensions) -> a -> m a

width, height ::
    (HasDimensions a)=> (Functor m)=>
    (X11.Dimension -> m X11.Dimension) -> a -> m a
width = dimensions . width'
  where width' f (Dimensions w h) = fmap (`Dimensions` h) (f w)
height = dimensions . height'
  where height' f (Dimensions w h) = fmap (Dimensions w) (f h)

instance HasDimensions Dimensions where
    dimensions = id

instance HasDimensions X11.Rectangle where
    dimensions f (X11.Rectangle x y w h) = fmap
        (\ (Dimensions w' h') -> X11.Rectangle x y w' h')
        (f (Dimensions w h))


class HasPosition a where
{-^ a class for types that are located in two dimensions -}
    position :: (Functor m)=> (X11.Point -> m X11.Point) -> a -> m a

pt_x, pt_y ::
    (HasPosition a)=> (Functor m)=>
    (X11.Position -> m X11.Position) -> a -> m a
pt_x = position . point_x
  where point_x f (X11.Point x y) = fmap (`X11.Point` y) (f x)
pt_y = position . point_y
  where point_y f (X11.Point x y) = fmap (X11.Point x) (f y)

instance HasPosition X11.Point where
   position = id

instance HasPosition X11.Rectangle where
    position f (X11.Rectangle x y w h) = fmap
        (\ (X11.Point x' y') -> X11.Rectangle x' y' w h)
        (f (X11.Point x y))


class HasWindow a where
    window :: (Functor m)=> (X11.Window -> m X11.Window) -> a -> m a

class HasBorder a where
    border :: (Functor m)=> (X11.Dimension -> m X11.Dimension) -> a -> m a
