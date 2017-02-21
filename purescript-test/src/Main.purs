module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff
import Ginger
import Data.Argonaut
import Data.StrMap as StrMap

main :: forall e. Eff (interpretGinger :: GINGER, console :: CONSOLE | e) Unit
main = do
  let template = "Hello, {{ name }}!\n{% for name, msg in items %}{{ name }} says: '{{ msg }}'\n{% endfor %}"
      context =  "name" := "sailor"
              ~> "items" :=
                    (  "Ginger" := "I have no soul"
                    ~> "Billy" := "Pang! Pang! You're dead!"
                    ~> jsonEmptyObject
                    )
              ~> jsonEmptyObject
  canceler <- runAff
    (log <<< show)
    (log <<< show) $ do
      result <- ginger template (encodeJson context)
      liftEff' $ log result
      result <- ginger "This is invalid syntax: {{" jsonEmptyObject
      liftEff' $ log result
      liftEff' $ log "All Done."
  pure unit
