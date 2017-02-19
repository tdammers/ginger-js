#!/bin/bash
INSTALL_ROOT=$(stack path --local-install-root)
PROJECT_NAME=ginger-js-export
JS_DIR="$INSTALL_ROOT/bin/$PROJECT_NAME.jsexe"
OUTPUT_DIR="npm-dist"
MODULE_NAME=ginger-js
OUTPUT_FILE="$OUTPUT_DIR/$MODULE_NAME"

mkdir -p "$OUTPUT_DIR"

(
    # Some serious callback voodoo is needed to play nice with nodejs.
    # The problem is that nodejs module exports happen synchronously, but
    # the Haskell code we use to create the `ginger` function is async. Hence,
    # the real `ginger` function is not available right away when the module
    # gets loaded, and trying to export it will export a null reference instead
    # of a function.
    #
    # To address this, we create an asynchronous wrapper that takes a callback
    # instead of returning the ginger output, and make it buffer requests in
    # an execution queue until the ginger function becomes available.
    # Then when the Haskell thread registers the real ginger function, the
    # wrapper switches its behavior from enqueueing to executing directly, and
    # also runs through the job queue. The callback setup allows us to do this
    # transparently and asynchronously.
    cat <<"EOT"
'use strict'
var actualGinger = null
var gingerQueue = []
var ginger = function (template, context, cb) {
  if (actualGinger == null)
    gingerQueue.push({template: template, context: context, cb: cb})
  else
    (function() {
        try {
            var result = actualGinger(template, context)
            cb(null, result)
        }
        catch (e) {
            cb(e, null)
        }
    })()
}
var registerGinger = function (g) {
  actualGinger = g
  var job = null
  while (job = gingerQueue.pop()) {
    (function() {
        try {
            var result = actualGinger(job.template, job.context)
            job.cb(null, result)
        }
        catch (e) {
            job.cb(e, null)
        }
    })()
  }
}
module.exports.ginger = ginger
EOT
    echo '/////////// BEGIN GHCJS GENERATED CODE ///////////'
    cat "$JS_DIR/all.js"
    echo '/////////// END GHCJS GENERATED CODE ///////////'
) > "$OUTPUT_FILE.js"
