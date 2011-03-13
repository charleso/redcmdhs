import Data.Maybe (fromJust)
import Network.HTTP (Response, getRequest, rspBody)
import Network.HTTP.Proxy (Proxy, fetchProxy)
import Network.Browser (browse, request, setAllowRedirects, setProxy)
import Network.URI (URI, parseURI)
import Control.Applicative ((<$>))

import Network.HTTP.Proxy

data Config = Config { 
    username :: String,
    password :: String,
    url :: URI,
    proxy :: Proxy
}

main = do
    p <- fetchProxy True
    (_, r) <- login (loadConfig { proxy = p })
    print r

loadConfig :: Config
loadConfig = Config {username = "", password = "", 
    url = fromJust $ parseURI "TODO",
    proxy = NoProxy
}

login :: Config -> IO (URI, Response String)
login config = browse $ do
    setAllowRedirects True
    setProxy $ proxy config
    request $ getRequest $ (show $ url config) ++ "Login/"
