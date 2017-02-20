module Ginger where

import Control.Monad.Aff
import FFI.Util.Function (callAff2r1)
import FFI.Util (require)
import Data.Argonaut.Core (Json)

foreign import data GINGER :: !

foreign import data GingerM :: *

gingerM :: GingerM
gingerM = require("../../bower_components/purescript-ginger/src/ginger-js")

ginger :: forall e
        . String
       -> Json
       -> Aff (interpretGinger :: GINGER | e) String
ginger template context =
  callAff2r1 gingerM "ginger" template context
