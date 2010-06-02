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
import Web.Encodings (encodeHtml)

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
                dir "new-post" $ methodSP GET $ do
                    renderPage dbh layout "admin-new-post" []
                ,
                dir "new-post" $ methodSP POST $ createNewPost dbh layout,
                dir "edit-post" $ path $ \postID -> methodSP GET $ do
                    rowM <- liftIO $ DB.rowMap dbh
                        "select * from posts where id = ?"
                        [DB.toSql postID]
                    when (isNothing rowM) $ fail "No post by that id exists"
                    let row = M.map DB.sqlAsString $ fromJust rowM
                    
                    renderPage dbh layout "admin-edit-post" [
                            "postID" ==> encodeHtml (postID :: String),
                            "postTitle" ==> encodeHtml (row M.! "title"),
                            "postDesc" ==> encodeHtml (row M.! "description"),
                            "postBody" ==> encodeHtml (row M.! "body")
                       ]
                ,
                dir "edit-post" $ methodSP POST $ editPost dbh layout,
                dir "remove-post"
                    $ path $ \postID -> path $ \sessionID -> methodSP GET
                    $ removePost dbh layout postID sessionID
                ,
                methodSP GET $ renderPage dbh layout "admin" []
            ]

createNewPost :: DB.IConnection conn =>
    conn -> Layout -> ServerPartT IO Response
createNewPost dbh layout = do
    title <- DB.toSql <$> ("title" `lookDefault` "")
    description <- DB.toSql <$> ("description" `lookDefault` "")
    body <- DB.toSql <$> ("body" `lookDefault` "")
    timestamp <- liftIO $ DB.toSql
        <$> formatTime defaultTimeLocale "%F %T"
        <$> getCurrentTime
    
    -- create random strings of hex until one of them happens to be unique
    postID <- liftIO $ msum $ repeat $ do
        postID' <- randHex 6
        DB.run dbh
            "insert into posts \
                \ (id, title, timestamp, body, description) \
                \ values (?,?,?,?,?)"
            [DB.toSql postID', title, timestamp, body, description]
        return postID'
    liftIO $ DB.commit dbh
    
    found (blogRoot layout ++ "/posts/" ++ postID) $
        toResponse "Forwarding to created post"

editPost :: DB.IConnection conn =>
    conn -> Layout -> ServerPartT IO Response
editPost dbh layout = do
    title <- DB.toSql <$> ("title" `lookDefault` "")
    description <- DB.toSql <$> ("description" `lookDefault` "")
    body <- DB.toSql <$> ("body" `lookDefault` "")
    postID <- "postID" `lookDefault` ""
    
    liftIO $ do
        DB.run dbh
            "update posts set title = ?, description = ?, body = ? where id = ?"
            [ title, description, body, DB.toSql postID ]
        DB.commit dbh
    
    found (blogRoot layout ++ "/posts/" ++ postID) $
        toResponse "Forwarding to edited post"

removePost :: DB.IConnection conn =>
    conn -> Layout -> String -> String -> ServerPartT IO Response
removePost dbh layout postID sessionID = do
    sessionCookie <- cookieValue <$> fromJust <$> maybeCookie "session"
    
    -- put session id in link so that posts can't be removed
    -- by malicious links and images and such
    when (sessionCookie /= sessionID) $
        fail "session mismatch"
    
    liftIO $ do
        DB.run dbh "delete from posts where id = ?" [ DB.toSql postID ]
        DB.commit dbh
    
    found (blogRoot layout ++ "/") $ toResponse "Forwarding to main page"
