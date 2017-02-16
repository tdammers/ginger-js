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
                     , toJSVal_pure
                     , ToJSVal
                     , FromJSVal
                     )
import GHCJS.Foreign.Callback
    ( Callback
    , syncCallback1'
    , syncCallback2'
    , OnBlocked(ContinueAsync)
    )
import Data.JSString (JSString, unpack, pack)
import Data.JSString.Text (textToJSString, textFromJSString, textFromJSVal)
import GHCJS.Types (JSVal, nullRef)
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
import Control.Monad (forM, forM_, forever, when)
import Control.Monad.IO.Class
import Control.Applicative
import Control.Arrow
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.IORef
import JavaScript.Object as O
import JavaScript.Object.Internal as OI

instance MonadIO m => ToGVal m JSVal where
    toGVal = jsvalToGVal

foreign import javascript unsafe "asText"
    js_asText :: JSVal -> JSVal

foreign import javascript unsafe "asListItems"
    js_asListItems :: JSVal -> JSVal

jsvalToList :: MonadIO m => JSVal -> Maybe [GVal m]
jsvalToList jsval =
    if [js'| Array.isArray(`jsval) |]
        then fmap jsvalToGVal <$> unsafePerformIO (fromJSVal . js_asListItems $ jsval)
        else Nothing

foreign import javascript unsafe "asDictItems"
    js_asDictItems :: JSVal -> JSVal

jsvalToDictItems :: MonadIO m => JSVal -> Maybe [(Text, GVal m)]
jsvalToDictItems jsval =
    if [js'| (typeof(`jsval) === 'object') && !(Array.isArray(`jsval)) |]
        then
            map (first textFromJSVal . second toGVal) <$>
                (unsafePerformIO (fromJSVal . js_asDictItems $ jsval) :: Maybe [(JSVal, JSVal)])
        else
            Nothing

jsvalToGVal :: MonadIO m => JSVal -> GVal m
jsvalToGVal jsval =
    def { asText = textFromJSVal . js_asText $ jsval
        , asHtml = toHtml . textFromJSVal . js_asText $ jsval
        , asNumber = Scientific.fromFloatDigits <$> ([js'|(typeof(`jsval) === 'number') ? `jsval : null|] :: Maybe Double)
        , asBoolean = [js'| Boolean(`jsval) |]
        , isNull = [js'|`jsval == null |]
        , asList = jsvalToList jsval
        , asLookup = Just $ \key -> toGVal <$> ([js'| `jsval[`key] |] :: Maybe JSVal)
        , asDictItems = jsvalToDictItems jsval
        , asFunction =
            if [js'|typeof(`jsval) === 'function'|]
                then
                    Just $ \args -> do
                        let argsStr = show args
                        jsargs <- liftIO . toJSVal . map (jsvalFromGVal . snd) $ args
                        jsvalToGVal <$> liftIO [js|`jsval.apply(`jsval, `jsargs) |]
                else
                    Nothing
        }

jsvalFromGVal :: GVal m -> JSVal
jsvalFromGVal gval = fromMaybe (unsafePerformIO . toJSVal $ asText gval) $
    (if isNull gval then Just nullRef else Nothing) <|>
    (jsvalFromGValDictItems <$> asDictItems gval) <|>
    (jsvalFromGValListItems <$> asList gval) <|>
    (jsvalFromGValNumber <$> asNumber gval) <|>
    Nothing

jsvalFromGValNumber :: Scientific -> JSVal
jsvalFromGValNumber x = case Scientific.floatingOrInteger x of
    Left f -> unsafePerformIO $ toJSVal (f :: Double)
    Right i -> unsafePerformIO $ toJSVal (i :: Int)

foreign import javascript unsafe "fromDictItems"
    js_fromDictItems :: JSVal -> JSVal

jsvalFromGValDictItems :: [(Text, GVal m)] -> JSVal
jsvalFromGValDictItems items =
    js_fromDictItems jsitems
    where
        jsitems :: JSVal
        jsitems = unsafePerformIO
                . toJSVal
                . map (first (unsafePerformIO . toJSVal) . second jsvalFromGVal)
                $ items

foreign import javascript unsafe "fromListItems"
    js_fromListItems :: JSVal -> JSVal

jsvalFromGValListItems :: [GVal m] -> JSVal
jsvalFromGValListItems items =
    js_fromListItems jsitems
    where
        jsitems :: JSVal
        jsitems = unsafePerformIO
                . toJSVal
                . map jsvalFromGVal
                $ items

ginger :: JSVal -> JSVal -> IO JSVal
ginger templateJS contextJS = do
    templateStr <- maybe (warn "Template must be string") return =<<
        fromJSVal templateJS
    template <- either (warn . show) return =<<
        parseGinger
            (const $ return Nothing) -- include resolver
            Nothing -- source name
            (Text.unpack templateStr) -- template source
    let context = fromMaybe [] $ jsvalToDictItems contextJS
    buf <- newIORef ""
    easyRenderM
        (\str -> modifyIORef buf (<> str))
        (HashMap.fromList context :: HashMap Text (GVal (Run IO Text)))
        template
    rendered <- readIORef buf
    toJSVal (rendered :: Text)

roundtrip :: JSVal -> IO JSVal
roundtrip jsval = do
    let gval :: GVal IO
        gval = toGVal jsval
    return $ jsvalFromGVal gval

foreign import javascript unsafe "registerGinger($1)"
    js_exportGingerNode :: Callback (JSVal -> JSVal -> IO JSVal) -> IO ()

foreign import javascript unsafe "ginger = $1"
    js_exportGingerBrowser :: Callback (JSVal -> JSVal -> IO JSVal) -> IO ()

foreign import javascript unsafe "console.log($1)"
    consoleLog :: JSString -> IO ()

warn :: String -> IO a
warn str = do
    consoleLog $ pack str
    fail str

main :: IO ()
main = do
    -- We need to detect whether we're running in node.js or not, because
    -- exporting and execution model differ between node and the browser.
    -- So we check whether 'module.exports' or 'window' exist; if the former,
    -- and not the latter, we're running node.
    if [js'| typeof(module) === 'object' && typeof(module.exports) === 'object' && typeof(window) === 'undefined' |]
        then do
            -- The following is required to keep the Haskell thread alive;
            -- without this kludge, node will exit immediately after
            -- require()ing the resulting module. However, keeping the Haskell
            -- thread blocked forever causes a runtime exception in the
            -- browser, so we'll only do it for node.js
            syncCallback2' ginger >>= js_exportGingerNode
            forever $ threadDelay 1000000
        else do
            syncCallback2' ginger >>= js_exportGingerBrowser
