:ramen: 🎮 miso-aframe 
====================

Integration of [aframe.io](https://aframe.io) with [miso](https://haskell-miso.org)

This repository serves as an example integration between miso and aframe, we can expand this to cover the entire library. aframe.io fits nicely with miso's virtual DOM abstraction.

```haskell
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
```

### Coverage

Ideally the entire [AFrame.io API](https://aframe.io/docs/) can be covered. Low-hanging fruit here.

  - [ ] [a-box](https://aframe.io/docs/1.7.0/primitives/a-box.html)
  - [ ] [a-camera](https://aframe.io/docs/1.7.0/primitives/a-camera.html)
  - [ ] [a-circle](https://aframe.io/docs/1.7.0/primitives/a-circle.html)
  - [ ] [a-cone](https://aframe.io/docs/1.7.0/primitives/a-cone.html)
  - [ ] [a-cubemap](https://aframe.io/docs/1.7.0/primitives/a-cubemap.html)
  - [ ] [a-cursor](https://aframe.io/docs/1.7.0/primitives/a-cursor.html)
  - [ ] [a-curvedimage](https://aframe.io/docs/1.7.0/primitives/a-curvedimage.html)
  - [ ] [a-cylinder](https://aframe.io/docs/1.7.0/primitives/a-cylinder.html)
  - [ ] [a-dodecahedron](https://aframe.io/docs/1.7.0/primitives/a-dodecahedron.html)
  - [ ] [a-gltf-model](https://aframe.io/docs/1.7.0/primitives/a-gltf-model.html)
  - [ ] [a-icosahedron](https://aframe.io/docs/1.7.0/primitives/a-icosahedron.html)
  - [ ] [a-image](https://aframe.io/docs/1.7.0/primitives/a-image.html)
  - [ ] [a-light](https://aframe.io/docs/1.7.0/primitives/a-light.html)
  - [ ] [a-link](https://aframe.io/docs/1.7.0/primitives/a-link.html)
  - [ ] [a-obj-model](https://aframe.io/docs/1.7.0/primitives/a-obj-model.html)
  - [ ] [a-octahedron](https://aframe.io/docs/1.7.0/primitives/a-octahedron.html)
  - [ ] [a-plane](https://aframe.io/docs/1.7.0/primitives/a-plane.html)
  - [ ] [a-ring](https://aframe.io/docs/1.7.0/primitives/a-ring.html)
  - [ ] [a-sky](https://aframe.io/docs/1.7.0/primitives/a-sky.html)
  - [ ] [a-sound](https://aframe.io/docs/1.7.0/primitives/a-sound.html)
  - [ ] [a-sphere](https://aframe.io/docs/1.7.0/primitives/a-sphere.html)
  - [ ] [a-tetrahedron](https://aframe.io/docs/1.7.0/primitives/a-tetrahedron.html)
  - [ ] [a-text](https://aframe.io/docs/1.7.0/primitives/a-text.html)
  - [ ] [a-torus-knot](https://aframe.io/docs/1.7.0/primitives/a-torus-knot.html)
  - [ ] [a-torus](https://aframe.io/docs/1.7.0/primitives/a-torus.html)
  - [ ] [a-triangle](https://aframe.io/docs/1.7.0/primitives/a-triangle.html)
  - [ ] [a-video](https://aframe.io/docs/1.7.0/primitives/a-video.html)
  - [ ] [a-videosphere](https://aframe.io/docs/1.7.0/primitives/a-videosphere.html)

### Development

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop
```

Once in the shell, you can call `cabal run` to start the development server and view the application at http://localhost:8080

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make"
```

### Build (JavaScript)

```bash
$ nix develop .#ghcjs --command bash -c "build"
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop .#wasm --command bash -c "serve"
```

### Clean

```bash
$ nix develop .#wasm --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
