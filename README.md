# Ginger For JavaScript

This is a wrapper that allows compiling
[Ginger](https://ginger.tobiasdammers.nl/) to JavaScript, using
[GHCJS](https://github.com/ghcjs/ghcjs).

It contains:

- Some Haskell and JavaScript boilerplate, including a `GVal` instance for
  GHCJS' `JSVal` type, which allows us to pass arbitrary JavaScript values to
  Ginger, and a `Main.hs` that pulls the main `ginger` function into scope
- Some extra boilerplate to make GHCJS' execution model play nice with NodeJS
  modules
- Stack and Cabal configuration
- A script to generate a NodeJS module from GHCJS compilation output
