module SlackStack.Handlers where
import SlackStack.Util
import qualified SlackStack.Util.DB as DB

import SlackStack.Handlers.Auth
import SlackStack.Handlers.Admin

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
        adminHandlers (layout root) dbh,
        methodSP GET $ postList root dbh,
        fileServe ["index.html"] "static"
    ]

postList :: DB.IConnection conn =>
    String -> conn -> ServerPartT IO Response
postList root dbh = do
    posts <- liftIO $ DB.rowMaps dbh
        "select * from posts order by timestamp desc" []
    renderPage dbh (layout root) "post-list" [
            "posts" ==> map (M.map DB.sqlAsString) posts
        ]
