-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Miso
import Miso.String (MisoString)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run (startApp app)
-----------------------------------------------------------------------------
app :: App () ()
app = (component () noop view_)
  { events = pointerEvents
#ifndef WASM
  , scripts = [ Src "https://aframe.io/releases/1.7.0/aframe.min.js" ]
#endif WASM
  }
-----------------------------------------------------------------------------
view_ :: () -> View () ()
view_ () =
  ascene_ []
    [ abox_
      [ position_ "-1 0.5 -3"
      , rotation_ "0 45 0"
      , color_ #4CC3D9
      ]
      []
    , asphere_
      [ position_ "0 1.25 -5"
      , radius_ 1.25
      , color_ #EF2D5E
      ]
      []
    , acylinder_
      [ position_ "1 0.75 -3"
      , radius_ 0.5
      , height_ "1.5"
      , color_ #FFC65D
      ]
      []
    , aplane_
      [ position_ "0 0 -4"
      , rotation_ "-90 0 0"
      , width_ "4"
      , height_ "4"
      , color_ #7BC8A4
      ]
      []
    , asky_
      [ color_ #ECECEC
      ]
      []
    ]
-----------------------------------------------------------------------------
ascene_ :: [Attribute action] -> [View parent action] -> View parent action
ascene_ = node HTML "a-scene"
-----------------------------------------------------------------------------
abox_ :: [Attribute action] -> [View parent action] -> View parent action
abox_ = node HTML "a-box"
-----------------------------------------------------------------------------
asphere_ :: [Attribute action] -> [View parent action] -> View parent action
asphere_ = node HTML "a-sphere"
-----------------------------------------------------------------------------
acylinder_ :: [Attribute action] -> [View parent action] -> View parent action
acylinder_ = node HTML "a-cylinder"
-----------------------------------------------------------------------------
aplane_ :: [Attribute action] -> [View parent action] -> View parent action
aplane_ = node HTML "a-plane"
-----------------------------------------------------------------------------
asky_ :: [Attribute action] -> [View parent action] -> View parent action
asky_ = node HTML "a-sky"
-----------------------------------------------------------------------------
position_ :: MisoString -> Attribute action
position_ = textProp "position"
-----------------------------------------------------------------------------
color_ :: MisoString -> Attribute action
color_ = textProp "color"
-----------------------------------------------------------------------------
rotation_ :: MisoString -> Attribute action
rotation_ = textProp "rotation"
-----------------------------------------------------------------------------
radius_ :: Double -> Attribute action
radius_ = doubleProp "radius"
-----------------------------------------------------------------------------
