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

import Text.Regex

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
        dir "about" $ methodSP GET $
            renderPage dbh (layout root) "about" []
        ,
        dir "posts" $ do
            -- /posts/3c0ff5/some-story-about-cabbage
            --       '——————'--> pid
            pid <- DB.toSql <$> head <$> rqPaths <$> askRq
            posts <- liftIO $ (withNextPrev dbh =<<) $ DB.rowMaps dbh
                "select * from posts where id = ? limit 1" [pid]
            renderPosts (layout root) dbh posts
        ,
        methodSP GET $ do
            posts <- liftIO $ DB.rowMaps dbh
                "select * from posts order by timestamp desc limit 5" []
            renderPosts (layout root) dbh posts
        ,
        fileServe ["index.html"] "static"
    ]

renderPosts :: DB.IConnection conn =>
    Layout -> conn -> [M.Map String DB.SqlValue] -> ServerPartT IO Response
renderPosts layout dbh posts = do
    let
        subRegex' re rep str = subRegex re str rep
        stripper =
            subRegex' (mkRegex "^-|-$") ""
            . subRegex' (mkRegex "[^A-Za-z0-9]+") "-"
        mapper m = M.insert "uri-title" (stripper $ m' M.! "title") m'
            where m' = M.map DB.sqlAsString m
        posts' = map mapper posts
    renderPage dbh layout "post-list" [
            "posts" ==> if null posts'
                then [ M.fromList [
                        ("body","Nothing to see here. Move along.")
                    ] ]
                else posts'
        ]
            
withNextPrev :: DB.IConnection conn =>
    conn -> [M.Map String DB.SqlValue] -> IO [M.Map String DB.SqlValue]
withNextPrev _ [] = return []
withNextPrev dbh posts = do
    nexts <- map (M.mapKeys ("next_" ++)) <$> DB.rowMaps dbh
        "select id,title from posts where timestamp > ? limit 1"
        [last posts M.! "timestamp"]
    prevs <- map (M.mapKeys ("prev_" ++)) <$> DB.rowMaps dbh
        "select id,title from posts where timestamp < ? limit 1"
        [last posts M.! "timestamp"]
    return $ init posts ++ [(f nexts) . (f prevs) $ last posts] where
        f xs = case xs of
            [] -> id
            [x] -> M.union x
