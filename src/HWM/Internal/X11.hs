{-
This module exists to provide a simplified interface to Xlib. There's too much there for me to keep in my head.
-}

module HWM.Internal.X11 where

import qualified Foreign.C.Types as C
import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Extras as X11


newtype XSession = XSession
   { _xDisplay :: X11.Display
   }


-- These functions are for accessing elements of the XSession record. Currently they just wrap functions from Graphics.X11, but in the future the XSession record may cache some of the values. If RandR can change these values, they can't be treated as pure like Graphics.X11 does. They have to be accessed through IO, and, if cached, cached in a mutable ref.

-- Basic operations (access members of Display struct):

-- XSession (Display) struct members:
_DefaultScreenNumber :: XSession -> X11.ScreenNumber
-- ^ Get Display.default_screen.
_DefaultScreenNumber = X11.defaultScreen . _xDisplay

_ScreenCount :: XSession -> C.CInt
-- ^ Get Display.nscreens.
_ScreenCount = X11.screenCount . _xDisplay

_ScreenOfNumber :: XSession -> X11.ScreenNumber -> X11.Screen
_ScreenOfNumber = X11.screenOfDisplay . _xDisplay


-- Screen struct members:
_DisplayOfScreen :: X11.Screen -> X11.Display
_DisplayOfScreen = X11.displayOfScreen

_RootWindowOfScreen :: X11.Screen -> X11.Window
_RootWindowOfScreen = X11.rootWindowOfScreen

_HeightOfScreen :: X11.Screen -> X11.Dimension
_HeightOfScreen = X11.heightOfScreen

_WidthOfScreen :: X11.Screen -> X11.Dimension
_WidthOfScreen = X11.widthOfScreen

_ScreenNumberOfScreen :: X11.Screen -> X11.ScreenNumber
_ScreenNumberOfScreen = X11.screenNumberOfScreen



-- Derived Functions:
_DefaultScreen :: XSession -> X11.Screen
-- ^ Get the Screen structure indexed by the default screen number.
_DefaultScreen = _ScreenOfNumber <*> _DefaultScreenNumber

_DefaultRootWindow :: XSession -> X11.Window
_DefaultRootWindow = _RootWindowOfScreen . _DefaultScreen

-- Use ScreenNumber rather than Screen:

_xScreenNumberHeight :: XSession -> X11.ScreenNumber -> X11.Dimension
_xScreenNumberHeight xSession screenNumber = _HeightOfScreen (_ScreenOfNumber xSession screenNumber)

_xScreenNumberWidth :: XSession -> X11.ScreenNumber -> X11.Dimension
_xScreenNumberWidth xSession screenNumber = _WidthOfScreen (_ScreenOfNumber xSession screenNumber)
