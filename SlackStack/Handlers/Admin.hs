module SlackStack.Handlers.Admin where

import SlackStack.Util
import Data.Maybe (fromJust,isJust,isNothing)
import qualified SlackStack.Util.DB as DB

import Happstack.Server
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans (liftIO)
import Control.Monad

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import System.Locale (defaultTimeLocale)

adminHandlers :: DB.IConnection conn =>
    Layout -> conn -> ServerPartT IO Response
adminHandlers layout dbh =
    dir "admin" $ do
        mIdentity <- getIdentity dbh
        when (isNothing mIdentity) $
            fail "This page requires identification"
        let (identity,level) = fromJust mIdentity
        when (level /= Root) $
            fail "Must be root-level to see this page"
        
        msum [
                dir "new-post" $ methodSP GET $ do
                    renderPage dbh layout "admin-new-post" []
                ,
                dir "new-post" $ methodSP POST $ newPost layout dbh,
                methodSP GET $ renderPage dbh layout "admin" []
            ]

newPost :: DB.IConnection conn =>
    Layout -> conn -> ServerPartT IO Response
newPost layout dbh = do
    body <- DB.toSql <$> ("body" `lookDefault` "")
    title <- DB.toSql <$> ("title" `lookDefault` "")
    timestamp <- liftIO $ DB.toSql
        <$> formatTime defaultTimeLocale "%F %T"
        <$> getCurrentTime
    
    pid <- liftIO $ msum $ repeat $ do
        pid' <- randHex 6
        DB.run dbh
            "insert into posts \
                \ (id, title, timestamp, body) \
                \ values (?,?,?,?)"
            [DB.toSql pid', title, timestamp, body]
        return pid'
    liftIO $ DB.commit dbh
    
    found (blogRoot layout ++ "/posts/" ++ pid) $
        toResponse "Forwarding to created post"
