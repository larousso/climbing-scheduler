
{-# LANGUAGE OverloadedStrings #-}

module Http where

import Control.Monad (forM_)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Blaze.ByteString.Builder as B
import Web.Scotty
import Web.Cookie

makeCookie :: BS.ByteString -> BS.ByteString -> SetCookie
makeCookie n v = def { setCookieName = n, setCookieValue = v }

renderSetCookie' :: SetCookie -> Text
renderSetCookie' = T.decodeUtf8 . B.toLazyByteString . renderSetCookie

setCookie :: BS.ByteString -> BS.ByteString -> ActionM ()
setCookie n v = setHeader "Set-Cookie" (renderSetCookie' (makeCookie n v))

getCookies :: ActionM (Maybe CookiesText)
getCookies =
    fmap (fmap (parseCookiesText . lazyToStrict . T.encodeUtf8)) $ header "Cookie"
    where
        lazyToStrict = BS.concat . BSL.toChunks
