module SlackStack.Handlers.Auth where

import SlackStack.Util
import Data.Maybe (fromJust,isNothing)
import qualified SlackStack.Util.DB as DB
import qualified Network.OpenID.Simple as ID

import Happstack.Server
import Data.ByteString.UTF8
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans (liftIO)
import Control.Monad (msum,mzero,mplus,when)

authHandlers :: DB.IConnection conn => conn -> ServerPartT IO Response
authHandlers dbh = msum [
        dir "openid" $ methodSP POST $ do
            identity <- fromJust <$> maybeLook "identity"
            
            sessionID <- liftIO $ randHex 32
            addCookie (60*60*24*7) $ mkCookie "session" sessionID
            
            host <- toString . fromJust <$> getHeaderM "Host"
            let returnTo = "http://" ++ host ++ "/openid"
            session <- liftIO $ ID.authenticate identity returnTo
            
            peerAddr <- fst <$> rqPeer <$> askRq
            liftIO $ do
                DB.run dbh
                    "insert into openid_sessions \
                    \ (id, addr, session) \
                    \ values (?, ?, ?)"
                    $ map DB.toSql [ sessionID, peerAddr, show session ]
                DB.commit dbh
            
            found (ID.authURI session) $
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
            
            uri <- rqQuery <$> askRq
            (=<< liftIO (ID.verify session uri)) $ \ok -> case ok of
                Just err -> fail err
                Nothing -> liftIO $ do
                    DB.run dbh
                        "delete from openid_sessions where id = ?"
                        [DB.toSql sessionID]
                    DB.run dbh
                        "insert into sessions (id,addr,identity) values (?,?,?)"
                        [DB.toSql sessionID, DB.toSql peerAddr, DB.toSql identity]
                    DB.commit dbh
            found "/" $ toResponse "Forwarding to main page"
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
                    addCookie 0 $ mkCookie "session" ""
                    found "/" $ toResponse "Logged out"
                else mzero
        ]
