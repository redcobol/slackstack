module SlackStack.Handlers.Admin where

import SlackStack.Util
import Data.Maybe (fromJust,isJust)
import qualified SlackStack.Util.DB as DB

import Happstack.Server
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans (liftIO)
import Control.Monad

adminHandlers :: DB.IConnection conn =>
    Layout -> conn -> ServerPartT IO Response
adminHandlers layout dbh = msum [
        dir "/admin" $ methodSP POST $ do
            mIdentity <- getIdentity dbh
            (guard $ isJust mIdentity) `mplus`
                fail "This page requires identification"
            let (identity,level) = fromJust mIdentity
            
            guard $ level == Root
            renderPage dbh layout "admin" []
    ]
