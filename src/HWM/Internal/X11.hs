{-
This module exists to provide a simplified interface to Xlib. There's too much there for me to keep in my head.

In practice, you get your Display, default Screen, and the root Window of your default Screen when you start up, and they never change. The only Screen you will ever use is the default one.

Don't export anything that explicitly uses or returns a Screen or Display.
-}


module HWM.Internal.X11 (
XSession, HasXSession (..), newDefaultXSession,
defaultRootWindow,
defaultGraphicsContext,
defaultVisual,
getDefaultRootWindowAttributes,
getWindowAttributes,
) where


import Control.Monad.Reader
import qualified Foreign.C.Types as C
import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Extras as X11
import qualified Graphics.X11.Xlib.Types as X11


newtype XSession = XSession
   { _XDisplay :: X11.Display
   }

-- XSession virtual members:
_XDefaultScreen :: XSession -> X11.Screen
_XDefaultScreen = X11.defaultScreenOfDisplay . _XDisplay

_XDefaultGraphicsContext :: XSession -> X11.GC
_XDefaultGraphicsContext = X11.defaultGCOfScreen . _XDefaultScreen

_XDefaultRootWindow :: XSession -> X11.Window
_XDefaultRootWindow = X11.rootWindowOfScreen . _XDefaultScreen

_XDefaultVisual :: XSession -> X11.Visual
_XDefaultVisual = X11.defaultVisualOfScreen . _XDefaultScreen

class HasXSession a where
   xSession :: a -> XSession

instance HasXSession XSession where
   xSession = id

-- These functions are for accessing elements of the XSession record. Currently they just wrap functions from Graphics.X11, but in the future the XSession record may cache some of the values. If RandR can change these values, they can't be treated as pure like Graphics.X11 does. They have to be accessed through IO, and, if cached, cached in a mutable ref.

-- Basic operations

newDefaultXSession :: IO XSession
newDefaultXSession = XSession `fmap` X11.openDisplay ""

-- Screen:
defaultScreen :: (HasXSession env, MonadReader env m)=> m X11.Screen
{-^ Get the default "Screen" of the "Display". (I.e. get the display adapter of the X session. Should not change unless you change which GPU you're using. I have no idea what happens then.)
-}
defaultScreen = asks (_XDefaultScreen . xSession)
{-# INLINE defaultScreen #-}

defaultRootWindow ::  (HasXSession env, MonadReader env m)=> m X11.Window
{-^ Get the "root Window" of the default "Screen" of the "Display". (I.e. get the identifier of the structure that contains pretty much all of the mutable information in the X session -- monitors, windows, supported extensions &c..)
-}
defaultRootWindow = asks (_XDefaultRootWindow . xSession)
{-# INLINE defaultRootWindow #-}

defaultGraphicsContext ::
    (HasXSession env, MonadReader env m)=>
    m X11.GC
defaultGraphicsContext = asks (_XDefaultGraphicsContext. xSession)
{-# INLINE defaultGraphicsContext #-}

defaultVisual ::
    (HasXSession env, MonadReader env m)=>
    m X11.Visual
defaultVisual = asks (_XDefaultVisual . xSession)
{-# INLINE defaultVisual #-}



-- Window:
getWindowAttributes ::
    (HasXSession env, MonadReader env m, MonadIO m)=>
    X11.Window -> m X11.WindowAttributes
getWindowAttributes window = do
    session <- asks $ _XDisplay . xSession
    liftIO $ X11.getWindowAttributes session window
{-# INLINABLE getWindowAttributes #-}

getDefaultRootWindowAttributes ::
    (HasXSession env, MonadReader env m, MonadIO m)=>
    m X11.WindowAttributes
getDefaultRootWindowAttributes = defaultRootWindow >>= getWindowAttributes
{-# INLINABLE getDefaultRootWindowAttributes #-}

selectInput ::
    (HasXSession env, MonadReader env m, MonadIO m)=>
    X11.Window -> X11.EventMask -> m ()
selectInput window eventMask = do
    session <- asks $ _XDisplay . xSession
    liftIO $ X11.selectInput session window eventMask

selectDefaultInput ::
    (HasXSession env, MonadReader env m, MonadIO m)=>
    X11.EventMask -> m ()
selectDefaultInput eventMask = defaultRootWindow >>= (`selectInput` eventMask)

sync :: (HasXSession env, MonadReader env m, MonadIO m)=> m ()
{-^ Flush the output buffer, then wait for all requests to be received and processed. Does not discard the events in the queue.
-}
sync =
    asks (_XDisplay . xSession) >>= \ session -> liftIO (X11.sync session False)

internAtom ::
    (HasXSession env, MonadReader env m, MonadIO m)=>
    String -> m X11.Atom
internAtom atom_name = asks (_XDisplay . xSession) >>= \ session -> liftIO (X11.internAtom session atom_name False)

getAtom ::
    (HasXSession env, MonadReader env m, MonadIO m)=>
    String -> m X11.Atom
getAtom atom_name = asks (_XDisplay . xSession) >>= \ session -> liftIO (X11.internAtom session atom_name True)

grabButton ::
    (HasXSession env, MonadReader env m, MonadIO m)=>
    X11.Button ->
    X11.ButtonMask ->
    X11.Window {-^ grab window -} ->
    Bool {-^ -} ->
    X11.EventMask ->
    X11.GrabMode ->
    X11.GrabMode ->
    X11.Window ->
    X11.Cursor ->
    m ()
grabButton button modifiers grab_window owner_events event_mask pointer_mode keyboard_mode confine_to cursor =
    asks (_XDisplay . xSession) >>= \ session ->
        liftIO (X11.grabButton session button modifiers grab_window owner_events event_mask pointer_mode keyboard_mode confine_to cursor)

-- grabButtonAsync ::
--     (HasXSession env, MonadReader env m, MonadIO m)=>
--     X11.Button ->
--     X11.ButtonMask ->
--     X11.Window ->
--     Bool ->
--     X11.EventMask ->
--     X11.Window ->
--     X11.Cursor ->
--     m ()
