import qualified OpenID.Easy as ID

main = do
    session <- ID.auth ID.config "substack.net" "http://sunspot:5050/"
    putStrLn $ ID.sAuthURI session
    putStr "forwarded back to uri: "
    uri <- getLine
    ID.verify session uri
