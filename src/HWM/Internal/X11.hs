{-
This module exists to provide a simplified interface to Xlib. There's too much there for me to keep in my head.
-}


module HWM.Internal.X11 where


import Control.Monad.Reader
import qualified Foreign.C.Types as C
import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Extras as X11
import qualified Graphics.X11.Xlib.Types as X11


newtype XSession = XSession
   { _XDisplay :: X11.Display
   }

class HasXSession a where
   xSession :: a -> XSession

instance HasXSession XSession where
   xSession = id

-- These functions are for accessing elements of the XSession record. Currently they just wrap functions from Graphics.X11, but in the future the XSession record may cache some of the values. If RandR can change these values, they can't be treated as pure like Graphics.X11 does. They have to be accessed through IO, and, if cached, cached in a mutable ref.

-- Basic operations

-- Screen:
defaultScreen :: (HasXSession env, MonadReader env m)=> m X11.Screen
defaultScreen = asks (X11.defaultScreenOfDisplay . _XDisplay . xSession)
-- defaultScreen = screenOfNumber <*> defaultScreenNumber
{-# INLINE defaultScreen #-}

defaultRootWindow ::  (HasXSession env, MonadReader env m)=> m X11.Window
defaultRootWindow = fmap _RootWindowOfScreen defaultScreen
{-# INLINE defaultRootWindow #-}

_DisplayOfScreen :: X11.Screen -> X11.Display
_DisplayOfScreen = X11.displayOfScreen

_RootWindowOfScreen :: X11.Screen -> X11.Window
_RootWindowOfScreen = X11.rootWindowOfScreen


-- Window:
getWindowAttributes ::
    (HasXSession env, MonadReader env m, MonadIO m)=>
    X11.Window -> m X11.WindowAttributes
getWindowAttributes window = do
    session <- asks $  _XDisplay . xSession
    liftIO $ X11.getWindowAttributes session window
{-# INLINABLE getWindowAttributes #-}
