#!/bin/bash
INSTALL_ROOT=$(stack path --local-install-root)
PROJECT_NAME=ginger-js-export
JS_DIR="$INSTALL_ROOT/bin/$PROJECT_NAME.jsexe"
MODULE_NAME=ginger-js

OUTPUT_FILE="$MODULE_NAME.js"

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
    echo '"use strict"'
    echo 'var actualGinger = null'
    echo 'var gingerQueue = []'
    echo 'var ginger = function (template, context, cb) {'
    echo '  if (actualGinger == null)'
    echo '    gingerQueue.push({template: template, context: context, cb: cb})'
    echo '  else'
    echo '    cb(actualGinger(template, context))'
    echo '}'
    echo 'var registerGinger = function (g) {'
    echo '  actualGinger = g'
    echo '  var job = null'
    echo '  while (job = gingerQueue.pop()) {'
    echo '    job.cb(g(job.template, job.context))'
    echo '  }'
    echo '}'
    echo 'module.exports.ginger = ginger'
    echo ''
    echo '/////////// BEGIN GHCJS GENERATED CODE ///////////'
    cat "$JS_DIR/all.js"
    echo '/////////// END GHCJS GENERATED CODE ///////////'
) > "$OUTPUT_FILE"
