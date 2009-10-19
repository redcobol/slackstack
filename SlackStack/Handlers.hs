module SlackStack.Handlers where
import SlackStack.Util

import Happstack.Server
import Web.Encodings (encodeHtml, encodeUrl)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Applicative
import qualified Data.Map as M

import Database.HDBC
import Database.HDBC.Sqlite3 (Connection)

layout :: String -> Layout
layout root = Layout {
    blogRoot = "",
    layoutPage = "layout",
    templateDir = root ++ "/templates",
    pageDir = root ++ "/templates/pages"
}

handlers :: String -> Connection -> ServerPartT IO Response
handlers root dbh = msum [
        --dir "openid" $ methodSP POST $ do
        --    readCookieValue "session",
        --    postList,
        methodSP GET $ postList root dbh,
        fileServe ["index.html"] "static"
    ]

postList :: String -> Connection -> ServerPartT IO Response
postList root dbh = do
    posts <- liftIO $ do
        sth <- prepare dbh "select * from posts order by timestamp desc"
        execute sth []
        fetchAllRowsMap sth
    
    renderPage (layout root) "post-list" [
            "title" ==> "The Universe of Discord",
            "posts" ==> map (M.map sqlAsString) posts,
            "categories" ==> ["comics", "blog"]
        ]
