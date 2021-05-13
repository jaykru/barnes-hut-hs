# Barnes-Hut-hs
Don't write numerical code in Haskell. Use something with good debugging tooling!

Let this repository serve as a warning to those naive enough to think they can get on productively without serious debugging tooling.

See the Common Lisp version of this project (coming soon!) for a parallel approach to Barnes-Hut.

------------

In this repository is an implementation in Haskell of the Barnes-Hut
[algorithm](https://en.wikipedia.org/wiki/Barnes%E2%80%93Hut_simulation) for
n-body gravitation simulation. Currently the algorithm computes n-body
gravitation forces but rendering the results (with SDL2) is WIP. There are
future plans to parallelize it using the Accelerate eDSL for CUDA.

# Dependencies
- `sdl2`
- `sdl2_gfx`
- latest stable `ghc`
- `cabal` build system

# Building
Install the deps and run `cabal build`

# Running
Run `cabal run`
