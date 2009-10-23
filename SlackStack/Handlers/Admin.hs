module SlackStack.Handlers.Admin where

import SlackStack.Util
import Data.Maybe (fromJust,isJust,isNothing)
import qualified SlackStack.Util.DB as DB

import Happstack.Server
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans (liftIO)
import Control.Monad

adminHandlers :: DB.IConnection conn =>
    Layout -> conn -> ServerPartT IO Response
adminHandlers layout dbh = msum [
        dir "admin" $ methodSP GET $ do
            mIdentity <- getIdentity dbh
            when (isNothing mIdentity) $
                fail "This page requires identification"
            
            let (identity,level) = fromJust mIdentity
            when (level /= Root) $
                fail "Must be root-level to see this page"
            
            renderPage dbh layout "admin" []
    ]
