{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module OpenID.Easy (
    Config(..),
    Session(..),
    auth, verify, config
) where

import Network.OpenID
import Network.Socket
import Control.Monad.Error
import Network.HTTP.Base
import Network.Stream (ConnError)
 
data Config = Config {
    verifyError :: String -> IO (),
    normalizeError :: IO Session,
    discoverError, associateError :: String -> IO Session
}

-- Provide default error handlers in the ErrorT monad which descriptive raise
-- string errors
config :: Config
config = Config {
    normalizeError = fail "Unable to normalize identifier",
    discoverError = fail . ("Discovery Error: " ++),
    associateError = fail . ("Associate Error: " ++),
    verifyError = fail . ("Verify Error: " ++)
}

data Session = Session {
    -- the authentication uri which the user clicks on as a string
    sAuthURI :: String,
    sProvider :: Provider,
    sIdentity :: Identifier,
    sConfig :: Config,
    sReturnTo :: String,
    sResolve :: Resolver IO,
    sAssocMap :: AssociationMap
}

-- Given an identity (such as substack.myopenid.com)
-- and a return-to uri (such as http://example.com/openid),
-- contact the remote authority to negotiate the openid transaction,
-- return 
--auth :: Config -> String -> String -> ErrorT String IO Session
auth :: Config -> String -> String -> IO Session
auth config ident returnTo = withSocketsDo $ do
    case normalizeIdentifier (Identifier ident) of
        Nothing -> normalizeError config
        Just normalizedIdent -> do
            let resolve = makeRequest True
            rpi <- discover resolve normalizedIdent
            case rpi of
                Left err -> discoverError config $ show err
                Right (provider,identifier) -> do
                    -- either an error or an association manager
                    eam <- associate emptyAssociationMap True resolve provider
                    case eam of
                        Left err -> associateError config $ show err
                        Right am ->
                            return $ Session {
                                sAuthURI = show $ authenticationURI
                                    am Setup provider identifier returnTo Nothing,
                                sProvider = provider,
                                sIdentity = identifier,
                                sConfig = config,
                                sReturnTo = returnTo,
                                sResolve = resolve,
                                sAssocMap = am
                            }

verify :: Session -> String -> IO ()
verify session uri = do
    let params = parseParams uri
    verified <- verifyAuthentication
        (sAssocMap session)
        params
        (sReturnTo session)
        (sResolve session)
    case verified of
        Left err -> verifyError (sConfig session) $ show err
        Right _ -> return ()
