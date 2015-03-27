% Writing Games in Haskell -- Beginner friendly intro
% Mihály Bárász, nilcons.com
% HaskellerZ meetup - March 26th, 2015

Outline
=======

- Lens recap

- Intro to `gloss`

- Actually writing the game

- Graphical and Game related libraries on Hackage

- Miscellaneous


Extremely brief intro to `lens`
=====

See: `lens-recap.hs`

Brief intro to `gloss`
=====

~~~ haskell
play :: Display                    -- ^ Window parameters
     -> Color                      -- ^ Background color
     -> Int                        -- ^ Simulation steps per secons
     -> world                      -- ^ The initial world
     -> (world -> Picture)         -- ^ “Rendering” function
     -> (Event -> world -> world)  -- ^ Event handler
     -> (Float -> world -> world)  -- ^ Simulation step function
     -> IO ()
~~~

The `Picture` type
=====

~~~ haskell
data Picture
    = Blank
    | Polygon   Path
    | Line      Path
    | Circle    Float
    | ThickCircle   Float Float
    | Arc       Float Float Float
    | ThickArc  Float Float Float Float

    | Text      String
    | Bitmap    Int Int     BitmapData Bool

    | Color     Color       Picture
    | Translate Float Float Picture
    | Rotate    Float       Picture
    | Scale     Float Float Picture

    | Pictures  [Picture]
~~~

Let's write us a game!
=====

See the `demo-game` subdirectory.

The End
=====

The actual presentation only got to this point. :)

Building the code
=====

Three ways to avoid recompiling `lens` over and over again and steer clear of “Cabal hell”.

- Nix -- Ceh ([shameless plug](https://github.com/nilcons/ceh))

- Nix -- Ollie's way: http://wiki.ocharles.org.uk/Nix

- Stackage snapshot in the global config

Basic packages
=====

- OpenGL: `OpenGL`, `gl`

- GLUT: `GLUT`

- GLFW: `GLFW-b`

- SDL 1.2: `SDL`, `SDL-{gfx,image,mixer,ttf}` (relatively complete)

- SDL 2.0: `sdl2` WIP

- sound: `OpenAL`, `ALUT`

- Engines: `helm`, `fwgl`, `HGamer3D`

Random thoughts
=====

- Temporal aliasing

- `gl` vs `OpenGL`  (Check out `quine`!)

What I didn't talk about -- FRP
=====

- **elerea**
- grapefruit
- **netwire**
- reactive
- reactive-banana
- sodium
- **yampa**


What I didn't talk about -- 3D
=====

- Modern OpenGL & GLSL

- Fun reading: [Deus Ex -- Rendering study](https://www.adriancourreges.com/blog/2015/03/10/deus-ex-human-revolution-graphics-study/)

- Bindings to “big” engines (Unreal4 or Unity)?

What I didn't talk about -- Sound
=====

- `SDL_mixer` bindings for `sdl2`

- OpenAL

- Generating sound effects (netwire; see O'Charles' asteroids game)

What I didn't talk about -- Portability
=====

Painless (relatively) building on Windows: [MinGHC](https://github.com/fpco/minghc)

Also see Haskell installation instructions on Stackage: http://www.stackage.org/install

What I didn't talk about -- Assets
=====





Thank you!
==========

Off topic: if you work a lot with date-time values in Haskell and are
annoyed with the `time` library, I'd like to hear from you!

For correct and efficient (and pure) handling of time zones check out
[hackage.haskell.org/package/tz](http://hackage.haskell.org/package/tz)
