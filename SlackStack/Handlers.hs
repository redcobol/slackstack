module SlackStack.Handlers where
import SlackStack.Util
import qualified SlackStack.Util.DB as DB

import Data.Maybe (fromJust,isJust,isNothing)

import Happstack.Server
import Web.Encodings (encodeHtml, encodeUrl)
import Control.Monad
import Control.Monad.Trans (lift,liftIO)
import Control.Applicative
import Control.Arrow
import qualified Data.Map as M
import Data.ByteString.UTF8 (toString)

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
            
            host <- toString . fromJust <$> getHeaderM "Host"
            let returnTo = "http://" ++ host ++ "/openid"
            session <- liftIO $ ID.auth ID.config identity returnTo
            
            peerAddr <- fst <$> rqPeer <$> askRq
            liftIO $ do
                DB.run dbh
                    "insert into openid_sessions \
                    \ (id, addr, session) \
                    \ values (?, ?, ?)"
                    $ map DB.toSql [ sessionID, peerAddr, show session ]
                DB.commit dbh
            
            found (ID.sAuthURI session) $
                toResponse "Forwarding to remote OpenID authority"
        ,
        dir "openid" $ do
            peerAddr <- fst <$> rqPeer <$> askRq
            
            mSessionID <- maybeCookie "session"
            when (isNothing mSessionID) $
                fail "Couldn't read session cookie"
            let sessionID = cookieValue $ fromJust mSessionID :: String
            
            mIdentity <- maybeLook "openid.identity"
            when (isNothing mIdentity) $
                fail "Couldn't read openid.identity parameter"
            let identity = fromJust mIdentity :: String
            
            mSession <- liftIO $ DB.rowList dbh
                "select session from openid_sessions \
                \ where id = ? and addr = ?"
                [DB.toSql sessionID, DB.toSql peerAddr]
            when (isNothing mSession) $ fail "session mismatch"
            let session = read $ DB.fromSql $ head $ fromJust mSession
            
            uri <- uncurry (++) . (rqUri &&& rqQuery) <$> askRq
            liftIO $ ID.verify ID.config session uri
            
            liftIO $ DB.run dbh
                "delete from openid_sessions where id = ?"
                [DB.toSql sessionID]
            
            liftIO $ DB.run dbh
                "insert into sessions (id,addr,identity) values (?,?,?)"
                [DB.toSql sessionID, DB.toSql peerAddr, DB.toSql identity]
            
            liftIO $ DB.commit dbh
            
            found (root ++ "/") $ toResponse "Forwarding to main page"
        ,
        do
            uri <- rqUri <$> askRq
            sessionID <- fromJust <$> (`mplus` Just "")
                <$> maybeCookieValue "session"
            -- put the session id in there to prevent people from linking to
            -- /logout/ in images or what-not
            if uri == "/logout/" ++ sessionID
                then do
                    liftIO $ do
                        DB.run dbh
                            "delete from sessions where id = ?"
                            [DB.toSql sessionID]
                        DB.commit dbh
                    found "/" $ toResponse "Logged out"
                else mzero
        ,
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
    
    renderPage (layout root) "post-list" [
            "title" ==> "The Universe of Discord",
            "posts" ==> map (M.map DB.sqlAsString) posts,
            "categories" ==> ["comics", "blog"],
            "identity" ==> fromJust mIdentity,
            "authed" ==> isJust mIdentity,
            "sessionID" ==> sessionID
        ]

getIdentity :: DB.IConnection conn =>
    conn -> ServerPartT IO (Maybe String)
getIdentity dbh = do
    peerAddr <- fst <$> rqPeer <$> askRq
    sessionID <- fromJust <$> (`mplus` Just "")
        <$> maybeCookieValue "session"
    
    liftIO $ (Just (DB.fromSql . head) <*>) <$> DB.rowList dbh
        "select identity from sessions \
        \ where id = ? and addr = ?"
        [DB.toSql sessionID, DB.toSql peerAddr]
