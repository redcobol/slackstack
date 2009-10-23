{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module SlackStack.Util where

import Happstack.Server
import Happstack.Util.Common

import Control.Monad
import Control.Monad.Trans (lift,liftIO)
import Control.Applicative

import Data.Maybe (fromJust)

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Web.Encodings (encodeHtml, encodeUrl)
import Safe (readMay)

import qualified Data.Map as M

import System.Random (randomRIO)
import Numeric (showHex)

asContentType :: String -> Response -> Response
asContentType cType res = res { rsHeaders = headers } where
    headers = setHeader "content-type" cType $ rsHeaders res

asHTML :: Response -> Response
asHTML = asContentType "text/html"

asText :: Response -> Response
asText = asContentType "text/plain"

asCSS :: Response -> Response
asCSS = asContentType "text/css"

-- syntactic sugar for setAttribute for hashier looking attribute composition
infixl 1 ==>
(==>) :: (ToSElem a, Stringable b) =>
    String -> a -> StringTemplate b -> StringTemplate b
(==>) = setAttribute

-- maybe read a query parameter, if this can be done
queryParse :: (ServerMonad m, MonadPlus m, MayReadString a) =>
    RqData String -> m (Maybe a)
queryParse rq = do
    param <- getDataFn rq
    return $ case param of
        Nothing -> Nothing
        Just x -> mayReadString x

-- maybe read a query param
maybeLook :: (ServerMonad m, MonadPlus m, MayReadString a) =>
    String -> m (Maybe a)
maybeLook name = queryParse $ look name

-- read a query parameter if possible, but otherwise return a fallback value
lookDefault :: (ServerMonad m, MonadPlus m, MayReadString a) =>
    String -> a -> m a
lookDefault name fallback = do
    param <- maybeLook name
    return $ fromJust $ param `mplus` Just fallback

-- maybe read a cookie
{-
maybeCookie :: (ServerMonad m, MonadPlus m, MayReadString a) =>
    String -> m (Maybe a)
maybeCookie name = queryParse $ readCookieValue name
-}
maybeCookie :: (ServerMonad m, MonadPlus m) => String -> m (Maybe Cookie)
maybeCookie name = getDataFn $ lookCookie name

maybeCookieValue :: (ServerMonad m, MonadPlus m) => String -> m (Maybe String)
maybeCookieValue name = do
    cookie <- getDataFn $ lookCookie name
    return $ case cookie of
        Nothing -> Nothing
        Just x -> Just $ cookieValue x

data Layout = Layout {
    blogRoot :: String,
    layoutPage :: String,
    templateDir :: String,
    pageDir :: String
}

-- render a page as a response given its template name in templates/pages/
-- and a list of attribute setters defined with (==>)
renderPage :: Layout -> String ->
    [ (StringTemplate String -> StringTemplate String) ] ->
    ServerPartT IO Response
renderPage layout page attr = do
    templates <- lift $ directoryGroup (templateDir layout)
    pages <- lift $ directoryGroup (pageDir layout)
    let
        pageT = fromJust $ getStringTemplate page pages
        layoutT = fromJust $ getStringTemplate (layoutPage layout) templates
        rendered = render $ foldl1 (.) attr pageT
        attr' =
            ("content" ==> rendered) .
            ("blogRoot" ==> blogRoot layout) .
            foldl1 (.) attr
    return $ asHTML $ toResponse $ render $ attr' layoutT

class (Read a) => MayReadString a where
    mayReadString :: String -> Maybe a
    mayReadString = readMay

instance MayReadString Int 
instance MayReadString Double 
instance MayReadString Float 

instance MayReadString Char where
    mayReadString (c:[]) = Just c
    mayReadString _ = Nothing

instance MayReadString String where
    mayReadString = Just . id

instance (MayReadString a) => MayReadString [a]
instance (MayReadString a, MayReadString b) => MayReadString (a,b)

randHex :: Integral a => a -> IO String
randHex size = (flip showHex $ "")
    <$> randomRIO (0, 2 ^ size :: Integer)
