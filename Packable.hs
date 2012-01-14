{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Packable where
import Graphics.UI.Gtk

-- packable objects
class WidgetClass w => Packable a w | a -> w where
    widgetOf :: a -> w

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

-- add widget to box with default params
boxPackS :: (BoxClass b, WidgetClass w, Packable a w) => b -> a -> Packing -> Int -> IO ()
boxPackS box child p i = boxPackStart box (widgetOf child) p i

--addToBox :: (BoxClass b, WidgetClass w, Packable a w) => b -> a -> IO ()
addToBox box widget = boxPackStart box (widgetOf widget) PackNatural 0
