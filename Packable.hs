{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Packable where
import Graphics.UI.Gtk
import Types

-- add widget to box
boxPackS :: (BoxClass b, WidgetClass w, Packable a w) => b -> a -> Packing -> Int -> IO ()
boxPackS box child p i = boxPackStart box (widgetOf child) p i

-- add with default params
addToBox box widget = boxPackS box widget PackNatural 0

-- packable objects
class WidgetClass w => Packable a w | a -> w where
    widgetOf :: a -> w

-- composite types

instance Packable EditorView VBox where
    widgetOf = displayBox

instance Packable PairView HBox where
    widgetOf = pvBox

instance Packable PairBox VBox where
    widgetOf = pbVbox

-- gtk widgets

instance Packable Button Button where
    widgetOf = id

instance Packable Entry Entry where
    widgetOf = id

instance Packable Label Label where
    widgetOf = id

instance Packable Notebook Notebook where
    widgetOf = id

instance Packable HBox HBox where
    widgetOf = id

