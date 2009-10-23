module SlackStack.Handlers where
import SlackStack.Util
import qualified SlackStack.Util.DB as DB

import SlackStack.Handlers.Auth

import Data.Maybe (fromJust,isJust,isNothing)

import Happstack.Server
import Web.Encodings (encodeHtml, encodeUrl)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Applicative
import Control.Arrow
import qualified Data.Map as M

layout :: String -> Layout
layout root = Layout {
    blogRoot = "",
    layoutPage = "layout",
    templateDir = root ++ "/templates",
    pageDir = root ++ "/templates/pages"
}

handlers :: DB.IConnection conn =>
    String -> conn -> ServerPartT IO Response
handlers root dbh = msum [
        authHandlers dbh,
        methodSP GET $ postList root dbh,
        fileServe ["index.html"] "static"
    ]

postList :: DB.IConnection conn =>
    String -> conn -> ServerPartT IO Response
postList root dbh = do
    posts <- liftIO $ DB.rowMaps dbh
        "select * from posts order by timestamp desc" []
    mIdentity <- getIdentity dbh
    sessionID <- fromJust <$> (`mplus` Just "")
        <$> maybeCookieValue "session"
    let (identity,level) = fromJust mIdentity
    
    renderPage (layout root) "post-list" [
            "title" ==> "The Universe of Discord",
            "posts" ==> map (M.map DB.sqlAsString) posts,
            "categories" ==> ["comics", "blog"],
            "identity" ==> identity,
            "isRoot" ==> level == Root,
            "isAuthed" ==> isJust mIdentity,
            "sessionID" ==> sessionID
        ]
