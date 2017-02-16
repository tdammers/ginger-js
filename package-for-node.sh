#!/bin/bash
INSTALL_ROOT=$(stack path --local-install-root)
PROJECT_NAME=ginger-js-export
JS_DIR="$INSTALL_ROOT/bin/$PROJECT_NAME.jsexe"
MODULE_NAME=ginger-js

OUTPUT_FILE="$MODULE_NAME.js"

(
    echo '"use strict"'
    echo ''
    echo '/////////// BEGIN GHCJS GENERATED CODE ///////////'
    cat "$JS_DIR/all.js"
    echo '/////////// END GHCJS GENERATED CODE ///////////'
) > "$OUTPUT_FILE"
