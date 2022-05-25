{-# LANGUAGE TypeSynonymInstances
           , DefaultSignatures
  #-}

module HWM.Internal.LibX11.Event where

import HWM.Internal.Optic
import HWM.Internal.LibX11.Lens
import Data.Function ((&))
import qualified Foreign.C.Types as C
import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Extras as X11
import qualified Graphics.X11.Xlib.Types as X11


{-
Every event has type :: CInt, serial :: ULong, send_event :: Bool, display :: Display.


window: Button, Key, Motion, Crossing, FocusChange, Visibility, CreateWindow, DistroyWindow, Unmap, Map, MapRequest, Reparent, Configure, Gravity, ResizeRequest, ConfigureRequest, Circulate, CirculateRequest, Property, SelectionClear, ClientMessage, Mapping
drawable: GraphicsExpose, NoExpose, Colormap
root: Button, Key, Motion, Crossing
subwindow: Button, Key, Motion, Crossing
parent: CreateWindow, MapRequest, Reparent, ConfigureRequest, CirculateRequest
event :: Window: DestroyWindow, Unmap, Map, Reparent, Configure, Gravity, Circulate
above :: Window: Configure, ConfigureRequest
owner :: Window: SelectionRequest
requestor :: Window: SelectionRequest, Selection
time: Button, Key, Motion, Crossing, Property, SelectionClear, SelectionRequest, Selection
x, y: Button, Key, Motion, Crossing, Expose, GraphicsExpose, CreateWindow, Reparent, Configure, Gravity, ConfigureRequest
x_root, y_root: Button, Key, Motion, Crossing
width, height: Expose, GraphicsExpose, CreateWindow, Configure, ResizeRequest, ConfigureRequest
mode: Crossing, FocusChange
detail: Crossing, FocusChange, ConfigureRequest
same_screen: Button, Key, Motion, Crossing
focus: Crossing
state: Button, Key, Motion, Crossing, Visibility, Property, Colormap
button: Button
keycode: Key
is_hint: Motion
major_code: GraphicsExpose, NoExpose
minor_code: GraphicsExpose, NoExpose
border_width: CreateWindow, Configure, ConfigureRequest
override_redirect: CreateWindow, Map, Reparent, Configure
from_configure: Unmap
value_mask: ConfigureRequest
place: CirculateEvent, CirculateRequest
atom: Property
selection: SelectionClear, SelectionRequest, Selection
target: SelectionRequest, Selection
property: SelectionRequest, Selection
message_type: ClientMessage
format: ClientMessage
data: ClientMessage
colormap: Colormap
new: Colormap
request: Mapping
first_keycode: Mapping
count: Mapping
char_vector: Keymap


But what events does the window manager NOT care about?
Colormap, ?Mapping?, ?Keymap?, ?SelectionClear?, ?SelectionRequest?, ?Selection?

What events does the window manager have to respond to?
CreateWindow, MapRequest, ConfigureRequest
(ResizeRequest is covered by ConfigureRequest.)
(You can probably ignore CirculateRequest and just let clients circulate as they want.)

window: Button, Key, Motion, Crossing, FocusChange, Visibility, CreateWindow, MapRequest, ConfigureRequest, Property, ClientMessage
root: Button, Key, Motion, Crossing
subwindow: Button, Key, Motion, Crossing
parent: CreateWindow, MapRequest, ConfigureRequest
above :: Window: ConfigureRequest
time: Button, Key, Motion, Crossing, Property
x, y: Button, Key, Motion, Crossing, CreateWindow, ConfigureRequest
x_root, y_root: Button, Key, Motion, Crossing
width, height: CreateWindow, ConfigureRequest
mode: Crossing, FocusChange
detail: Crossing, FocusChange, ConfigureRequest
same_screen: Button, Key, Motion, Crossing
focus: Crossing
state: Button, Key, Motion, Crossing, Visibility, Property
button: Button
keycode: Key
is_hint: Motion
border_width: CreateWindowe, ConfigureRequest
override_redirect: CreateWindow
value_mask: ConfigureRequest
atom: Property
message_type: ClientMessage
format: ClientMessage
data: ClientMessage

What events does the window manager produce?

-}


data AnyEvent = AnyEvent
    { _AnyEvent_serial :: !C.CULong
    , _AnyEvent_send_event :: !Bool
    , _AnyEvent_window :: !X11.Window
    }

class HasAnyEvent a where
    anyEvent :: (Functor m)=> (AnyEvent -> m AnyEvent) -> a -> m  a

instance HasAnyEvent AnyEvent where
    anyEvent = id

instance HasWindow AnyEvent where
    window f s = fmap
        (\ window' -> s{ _AnyEvent_window = window' })
        (f (_AnyEvent_window s))

serial :: (HasAnyEvent a)=> (Functor m)=> (C.CULong -> m C.CULong) -> a -> m a
serial = anyEvent . serialL
   where
     serialL f s = fmap
         (\ serial' -> s{ _AnyEvent_serial = serial' })
         (f (_AnyEvent_serial s))

sent_event :: (HasAnyEvent a)=> a -> Bool
sent_event = (^. anyEvent . to _AnyEvent_send_event)

data Event
    = EventRequest Request
    | EventNotify Notify
    | EventMessage Message

data Request
    = RequestConfigure !ConfigureRequest
    | RequestMap !MapRequest

instance HasAnyEvent Request where
    anyEvent f (RequestConfigure s) = fmap RequestConfigure (anyEvent f s)
    anyEvent f (RequestMap s) = fmap RequestMap (anyEvent f s)

instance HasWindow Request where
    window = anyEvent . window

data ConfigureRequest = ConfigureRequest
   { _ConfigureRequest_AnyEvent :: !AnyEvent
   , _ConfigureRequest_parent :: !X11.Window
   , _ConfigureRequest_above :: !X11.Window
   , _ConfigureRequest_position :: !X11.Point
   , _ConfigureRequest_dimensions :: !Dimensions
   , _ConfigureRequest_border_width :: !X11.Dimension
   , _ConfigureRequest_detail :: !X11.NotifyDetail
   , _ConfigureRequest_value_mask :: !C.CULong
   }

instance HasAnyEvent ConfigureRequest where
    anyEvent f s = fmap
        (\ anyEvent' -> s{ _ConfigureRequest_AnyEvent = anyEvent' })
        (f (_ConfigureRequest_AnyEvent s))

data MapRequest = MapRequest
    { _MapRequest_AnyEvent :: !AnyEvent
    , _MapRequest_parent :: !X11.Window
    }

instance HasAnyEvent MapRequest where
    anyEvent f s = fmap
        (\ anyEvent' -> s{ _MapRequest_AnyEvent = anyEvent' })
        (f (_MapRequest_AnyEvent s))

data Notify
    = NotifyConfigure !ConfigureNotify
    | NotifyMap !MapNotify
    | NotifyUnmap !UnmapNotify
    | NotifyDestroyWindow !DestroyWindowNotify
    | NotifyExpose !ExposeNotify
    | NotifyPressKey !KeyNotify
    | NotifyReleaseKey !KeyNotify
    | NotifyPressButton !ButtonNotify
    | NotifyReleaseButton !ButtonNotify
    | NotifyMotion !MotionNotify
    | NotifyEnterWindow !CrossingNotify
    | NotifyLeaveWindow !CrossingNotify

data ConfigureNotify = ConfigureNotify
   { _ConfigureNotify_AnyEvent :: !AnyEvent
   , _ConfigureNotify_event :: !X11.Window
   , _ConfigureNotify_position :: !X11.Point
   , _ConfigureNotify_dimensions :: !Dimensions
   , _ConfigureNotify_border_width :: !X11.Dimension
   , _ConfigureNotify_override_redirect :: !Bool
   }

data MapNotify = MapNotify
    { _MapNotify_AnyEvent :: !AnyEvent
    , _MapNotify_event :: !X11.Window
    , _MapNotify_override_redirect :: !Bool
    }

data UnmapNotify = UnmapNotify
    { _UnmapNotify_AnyEvent :: !AnyEvent
    , _UnmapNotify_event :: !X11.Window
    , _UnmapNotify_from_configure :: !Bool
    }

data DestroyWindowNotify = DestroyWindowNotify
   { _DestroyWindowNotify_AnyEvent :: !AnyEvent
   , _DestroyWindowNotify_event :: !X11.Window
   }

data ExposeNotify = ExposeNotify
    { _ExposeNotify_AnyEvent :: !AnyEvent
    , _ExposeNotify_position :: !X11.Point
    , _ExposeNotify_dimensions :: !Dimensions
    , _ExposeNotify_count :: !C.CInt
    }

data KeyNotify = KeyNotify
   { _KeyNotify_AnyEvent :: !AnyEvent
   , _KeyNotify_subwindow :: !X11.Window
   , _KeyNotify_time :: !X11.Time
   , _KeyNotify_position :: !X11.Point
   , _KeyNotify_root_position :: !X11.Point
   , _KeyNotify_state :: !X11.KeyMask
   , _KeyNotify_keycode :: !X11.KeyCode
   }

data ButtonNotify = ButtonNotify
   { _ButtonNotify_AnyEvent :: !AnyEvent
   , _ButtonNotify_subwindow :: !X11.Window
   , _ButtonNotify_time :: !X11.Time
   , _ButtonNotify_position :: !X11.Point
   , _ButtonNotify_root_position :: !X11.Point
   , _ButtonNotify_state :: !X11.KeyMask
   , _ButtonNotify_button :: !X11.Button
   }

data MappingNotify = MappingNotify
    { _MappingNotify_AnyEvent :: !AnyEvent
    , _MappingNotify_request :: !X11.MappingRequest
    , _MappingNotify_first_keycode :: !X11.KeyCode
    , _MappingNotify_count :: !C.CInt
    }

data CrossingNotify = CrossingNotify
    { _CrossingNotify_AnyEvent :: !AnyEvent
    , _CrossingNotify_subwindow :: !X11.Window
    , _CrossingNotify_time :: !X11.Time
    , _CrossingNotify_position :: !X11.Point
    , _CrossingNotify_root_position :: !X11.Point
    , _CrossingNotify_mode :: !X11.NotifyMode
    , _CrossingNotify_detail :: !X11.NotifyDetail
    , _CrossingNotify_focus :: !Bool
    , _CrossingNotify_state :: !X11.Modifier
    }

data MotionNotify = MotionNotify
   { _MotionNotify_AnyEvent :: !AnyEvent
   , _MotionNotify_position :: !X11.Point
   }

data RRScreenChangeNotify = RRScreenChangeNotify
    { _RRScreenChangeNotify_root :: !X11.Window
    , _RRScreenChangeNotify_timestamp :: !X11.Time
    , _RRScreenChangeNotify_config_timestamp :: !X11.Time
    , _RRScreenChangeNotify_size_index :: !X11.SizeID
    , _RRScreenChangeNotify_subpixel_order :: !X11.SubpixelOrder
    , _RRScreenChangeNotify_rotation :: !X11.Rotation
    , _RRScreenChangeNotify_dimensions :: !Dimensions
    , _RRScreenChangeNotify_mwidth :: !C.CInt
    , _RRScreenChangeNotify_mheight :: !C.CInt
    }

data Message
    = EventClientMessage !ClientMessage

data ClientMessage = ClientMessage
    { _ClientMessage_AnyEvent :: !AnyEvent
    , _ClientMessage_message_type :: !X11.Atom
    , _ClientMessage_data :: ![C.CInt]
    }
