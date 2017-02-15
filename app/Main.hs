{-#LANGUAGE JavaScriptFFI #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Main where

import Text.Ginger
import Text.Ginger.Html (toHtml)
import GHCJS.Marshal ( fromJSVal
                     , toJSVal
                     , FromJSVal
                     )
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
import GHCJS.Foreign.QQ
import Data.Default (def)
import Data.Scientific as Scientific
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM)

foreign import javascript unsafe "console.log($1)"
    consoleLog :: JSString -> IO ()

foreign import javascript unsafe "console.log($1)"
    consoleLogRaw :: JSVal -> IO ()

warn :: String -> IO a
warn str = do
    consoleLog $ pack str
    fail str

instance ToGVal m JSVal where
    toGVal = jsvalToGVal

jsvalToGVal :: JSVal -> GVal m
jsvalToGVal jsval =
    def { asText = Text.pack . unpack $ [js'|`jsval + ''|]
        , asHtml = toHtml . Text.pack . unpack $ [js'|`jsval + ''|]
        , asNumber = Scientific.fromFloatDigits <$> ([js'|(typeof(`jsval) === 'number') ? `jsval : null|] :: Maybe Double)
        , asBoolean = [js'| Boolean(`jsval) |]
        , isNull = [js'|`jsval == null |]
        , asList = map toGVal <$>
            (unsafePerformIO (fromJSVal [js'| (Array.isArray(`jsval)) ? `jsval : null |]) :: Maybe [JSVal])
        , asLookup = Just $ \key -> toGVal <$> ([js'| `jsval[`key] |] :: Maybe JSVal)
        , asDictItems = do
            keys <- (unsafePerformIO . fromJSVal) [js'| 
                    Object.isExtensible(`jsval) ? 
                        Object.getOwnPropertyNames(`jsval) :
                        null |]
            values <- forM keys $ \key -> toGVal <$> ([js'| `jsval[`key] |] :: Maybe JSVal)
            Just $ zip keys values
        }

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
            (HashMap.fromList context :: HashMap Text JSVal)
            template
    toJSVal rendered

foreign import javascript unsafe "ginger = $1"
    js_exportGinger :: Callback (JSVal -> JSVal -> IO JSVal) -> IO ()

main :: IO ()
main = do
    callback <- syncCallback2' $ \templateJSVal contextJSVal -> do
        ginger templateJSVal contextJSVal
    js_exportGinger callback
