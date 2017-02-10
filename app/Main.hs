{-#LANGUAGE JavaScriptFFI #-}
{-#LANGUAGE OverloadedStrings #-}
module Main where

import Text.Ginger
import GHCJS.Marshal(fromJSVal, toJSVal)
import GHCJS.Foreign.Callback
    ( Callback
    , syncCallback2'
    , OnBlocked(ContinueAsync)
    )
import Data.JSString (JSString, unpack, pack)
import GHCJS.Types (JSVal)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson (Value)
import System.IO (hPutStrLn, stderr)

foreign import javascript unsafe "console.log($1)"
    consoleLog :: JSString -> IO ()

foreign import javascript unsafe "console.log($1)"
    consoleLogRaw :: JSVal -> IO ()

warn :: String -> IO a
warn str = do
    consoleLog $ pack str
    fail str

ginger :: JSVal -> JSVal -> IO JSVal
ginger templateJS contextJS = do
    templateStr <- maybe (warn "Template must be string") return =<<
        fromJSVal templateJS
    template <- either (warn . show) return =<<
        parseGinger
            (const $ return Nothing) -- include resolver
            Nothing -- source name
            (Text.unpack templateStr) -- template source
    context <- maybe (warn "Context must be object") return =<<
        fromJSVal contextJS
    let rendered :: Text
        rendered = easyRender
            (HashMap.fromList context :: HashMap Text Value)
            template
    toJSVal rendered

foreign import javascript unsafe "ginger = $1"
    js_exportGinger :: Callback (JSVal -> JSVal -> IO JSVal) -> IO ()

main :: IO ()
main = do
    callback <- syncCallback2' $ \templateJSVal contextJSVal -> do
        ginger templateJSVal contextJSVal
    js_exportGinger callback
