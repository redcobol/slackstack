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

import qualified Data.Map as M

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
                dir "edit-post" $ path $ \postID -> methodSP GET $ do
                    rowM <- liftIO $ DB.rowMap dbh
                        "select * from posts where id = ?"
                        [DB.toSql postID]
                    when (isNothing rowM) $ fail "No post by that id exists"
                    let row = M.map DB.sqlAsString $ fromJust rowM
                    
                    renderPage dbh layout "admin-edit-post" [
                            "postID" ==> (postID :: String),
                            "postTitle" ==> row M.! "title",
                            "postBody" ==> row M.! "body"
                       ]
                ,
                dir "edit-post" $ methodSP POST $ editPost layout dbh,
                dir "new-post" $ methodSP GET $ do
                    renderPage dbh layout "admin-new-post" []
                ,
                dir "new-post" $ methodSP POST $ createNewPost layout dbh,
                methodSP GET $ renderPage dbh layout "admin" []
            ]

createNewPost :: DB.IConnection conn =>
    Layout -> conn -> ServerPartT IO Response
createNewPost layout dbh = do
    body <- DB.toSql <$> ("body" `lookDefault` "")
    title <- DB.toSql <$> ("title" `lookDefault` "")
    timestamp <- liftIO $ DB.toSql
        <$> formatTime defaultTimeLocale "%F %T"
        <$> getCurrentTime
    
    -- create random strings of hex until one of them happens to be unique
    postID <- liftIO $ msum $ repeat $ do
        postID' <- randHex 6
        DB.run dbh
            "insert into posts \
                \ (id, title, timestamp, body) \
                \ values (?,?,?,?)"
            [DB.toSql postID', title, timestamp, body]
        return postID'
    liftIO $ DB.commit dbh
    
    found (blogRoot layout ++ "/posts/" ++ postID) $
        toResponse "Forwarding to created post"

editPost :: DB.IConnection conn =>
    Layout -> conn -> ServerPartT IO Response
editPost layout dbh = do
    body <- DB.toSql <$> ("body" `lookDefault` "")
    title <- DB.toSql <$> ("title" `lookDefault` "")
    postID <- "post_id" `lookDefault` ""
    
    liftIO $ DB.run dbh
        "update posts set title = ?, body = ? where id = ?"
        [ title, body, DB.toSql postID ]
    
    found (blogRoot layout ++ "/posts/" ++ postID) $
        toResponse "Forwarding to edited post"
