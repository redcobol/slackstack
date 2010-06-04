module SlackStack.Handlers where
import SlackStack.Util
import qualified SlackStack.Util.DB as DB

import SlackStack.Handlers.Auth
import SlackStack.Handlers.Admin
import SlackStack.Handlers.RSS

import Data.Maybe (fromJust,isJust,isNothing)

import Happstack.Server
import Web.Encodings (encodeHtml, encodeUrl)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Applicative
import Control.Arrow
import qualified Data.Map as M

import Text.Regex

import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)

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
        rssHandler dbh,
        authHandlers dbh,
        adminHandlers (layout root) dbh,
        dir "about" $ methodSP GET $
            renderPage dbh (layout root) "about" [
                "title" ==> "About Me!"
            ]
        ,
        dir "posts" $ do
            -- /posts/3c0ff5/some-story-about-cabbage
            --       '——————'--> pid
            pid <- DB.toSql . head . rqPaths <$> askRq
            posts <- liftIO $ (withNextPrev dbh 0 =<<) $ DB.rowMaps dbh
                "select * from posts where id = ? limit 1" [pid]
            renderPosts (layout root) dbh posts
        ,
        dir "browse" $ do
            let lastF xs = case xs of { [] -> ""; xs -> last xs }
            dateP <- lastF . rqPaths <$> askRq
            let parseT f = parseTime defaultTimeLocale f dateP
            posts <- liftIO $ (withNextPrev dbh 5 =<<)
                $ case parseT "%F %T" <|> parseT "%F" of
                    Nothing -> DB.rowMaps dbh
                            "select * from posts order by timestamp desc limit 5" []
                    Just date -> DB.rowMaps dbh
                        "select * from posts where timestamp <= ?\
                        \order by timestamp desc limit 5"
                        [DB.toSql (date :: UTCTime)]
            renderPosts (layout root) dbh posts
        ,
        methodSP GET $ do
            posts <- liftIO $ (withNextPrev dbh 5 =<<) $ DB.rowMaps dbh
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
        mTitle = case posts of
            [post] -> (:[]) . ("title" ==>)
                $ (DB.sqlAsString $ post M.! "title")
            _ -> []
    renderPage dbh layout "post-list" $ mTitle ++ [
            "single" ==> length posts == 1,
            "posts" ==> case posts' of
                [] -> [M.fromList[("body","Nothing to see here. Move along.")]]
                xs -> xs
        ]
            
withNextPrev :: DB.IConnection conn
    => conn -> Int -> [M.Map String DB.SqlValue]
    -> IO [M.Map String DB.SqlValue]
withNextPrev _ _ [] = return []
withNextPrev dbh n posts = do
    let dt = last posts M.! "timestamp"
    nexts <- map (M.mapKeys ("next_" ++)) <$> DB.rowMaps dbh
        "select id,title,timestamp from posts \
        \where timestamp > ? order by timestamp asc limit 1 offset ?"
        [dt, DB.toSql n]
    prevs <- map (M.mapKeys ("prev_" ++)) <$> DB.rowMaps dbh
        "select id,title,timestamp from posts \
        \where timestamp < ? order by timestamp desc limit 1"
        [dt]
    return $ init posts ++ [(f nexts) . (f prevs) $ last posts] where
        f xs = case xs of
            [] -> id
            [x] -> M.union x
