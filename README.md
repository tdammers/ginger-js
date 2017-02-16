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

## Installation

### Requirements

- git
- [stack](https://haskellstack.org/)
- bash

### Steps

- Clone the ginger-js repository:

  ```sh
  git clone https://github.com/tdammers/ginger-js
  cd ginger-js
  ```

- Pull in Haskell dependencies (including the compiler):

  ```sh
  stack init
  ```

- Build the Haskell project:

  ```sh
  stack install
  ```

At this point, you should have a working JavaScript version of Ginger somewhere
under `.stack-work/install`; the path should look something like
`.stack-work/install/x86_64-linux/lts-7.14/ghcjs-0.2.1.9007014_ghc-8.0.1/bin/ginger-js-export.jsexe`,
and it contains an `index.html` file that you can load into a browser to try
out Ginger in the browser console.

In order to turn this into a usable NodeJS module, run the
`package-for-node.sh` script in the project root, which will create the file
`ginger-js.js`. This file is a proper CommonJS module, suitable for
`require()`ing in NodeJS.

## Usage

`ginger-js` creates one function:

`function ginger(template, context, cb)`

In NodeJS, this function is available as a module export
(`require('ginger-js').ginger`), in the browser it is just a global variable
(`window.ginger`).

### Arguments

- `template` is the template to use, as a string (it is not currently possible
  to run the compilation step separately)
- `context` is an object representing the execution context; properties of this
  object become template variables.
- `cb(output)` is a callback that receives the rendered template output, as a
  string.

### Limitations

- Currently, the JS version does not supply any loader for includes, thus `{%
  include %}` and `{% extends %}` are not available.
- There are no provisions for specifying the output encoding, only plain text
  output is currently available.
