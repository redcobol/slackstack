module SlackStack.Handlers where
import SlackStack.Util
import qualified SlackStack.Util.DB as DB

import Data.Maybe (fromJust,isJust)
import System.Environment (getEnv)

import Happstack.Server
import Web.Encodings (encodeHtml, encodeUrl)
import Control.Monad
import Control.Monad.Trans (lift,liftIO)
import Control.Applicative
import qualified Data.Map as M

import qualified Network.OpenID.Easy as ID

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
        dir "openid" $ methodSP POST $ do
            identity <- fromJust <$> maybeLook "identity"
            
            sessionID <- liftIO $ randHex 256
            addCookie (60*60*24*7) $ mkCookie "session" sessionID
            
            returnTo <- liftIO $
                ("http://" ++) . (++ "/openid") <$> getEnv "HTTP_HOST"
            session <- liftIO $ ID.auth ID.config identity returnTo
            
            addr <- liftIO $ getEnv "REMOTE_ADDR"
            liftIO $ DB.run dbh
                    "insert into openid_sessions \
                    \ (id, addr, session) \
                    \ values (?, ?, ?)"
                $ map DB.toSql [ sessionID, addr, show session ]
            
            setHeaderM "Location" $ ID.sAuthURI session
            return $ toResponse
                $ "Your browser should forward you along to "
                    ++ ID.sAuthURI session
        ,
        dir "openid" $ do
            sessionID <- fromJust <$> maybeCookie "session"
                :: (ServerMonad m, MonadPlus m, Functor m) => m String
            
            addr <- liftIO $ getEnv "REMOTE_ADDR"
            identity <- fromJust <$> maybeLook "openid.identity"
            
            session <- liftIO $ read . DB.fromSql . head . fromJust
                <$> (DB.rowList dbh
                    "select session from openid_sessions \
                    \ where identity = ? and addr = ?"
                    $ map DB.toSql [identity,addr])
            
            uri <- liftIO $ getEnv "REQUEST_URI"
            liftIO $ ID.verify ID.config session uri
            
            liftIO $ DB.run dbh
                "delete from openid_sessions where id = ?"
                [DB.toSql sessionID]
            
            liftIO $ DB.run dbh
                "insert into sessions (id,addr,identity) values (?,?,?)"
                [DB.toSql sessionID, DB.toSql addr, DB.toSql identity]
            
            setHeaderM "Location" "/"
            return $ toResponse "Forwarding to /"
        ,
        methodSP GET $ postList root dbh,
        fileServe ["index.html"] "static"
    ]

postList :: DB.IConnection conn =>
    String -> conn -> ServerPartT IO Response
postList root dbh = do
    posts <- liftIO $ DB.rowMaps dbh
        "select * from posts order by timestamp desc" []
    renderPage (layout root) "post-list" [
            "title" ==> "The Universe of Discord: ",
            "posts" ==> map (M.map DB.sqlAsString) posts,
            "categories" ==> ["comics", "blog"]
        ]
