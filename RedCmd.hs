{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Data.Maybe (fromJust)
import Network.Curl (curlGet)
import Network.Curl.Opts
import Network.URI (URI, parseURI)

import Text.HTML.TagSoup (parseTags)
import Text.XML.HXT.Core
import Text.XML.HXT.TagSoup (withTagSoup)

import Data.Tree

data Config = Config { 
    username :: String,
    password :: String,
    url :: URI
}

loadConfig :: Config
loadConfig = Config {username = "", password = "", 
    url = fromJust $ parseURI "TODO"
}

main = runX $ parse "test/issue1.html" >>> parseIssue

parse = readDocument [ 
        withParseHTML yes, 
        withWarnings no, 
        withTagSoup 
    ]

data Issue = Issue { subject :: String, description :: String, history :: [History] }
    deriving Show

data History = History { note :: String } deriving Show

parseIssue = proc x -> do
    subject <- getIssueField "subject" -< x
    desc <- getIssueFieldD "description" -< x
    historyT <- deep (hasAttrValue "id" (== "history")) -< x
    history <- listA parseHistory -< historyT
    returnA -< Issue { 
        subject = subject,
        description = desc,
        history = history
    }

atTag tag = deep (isElem >>> hasName tag)

parseHistory = deep (
    hasAttrValue "class" (== "journal") >>>
    proc x -> do
        note <- getText <<< getChildren <<< atTag "p" <<< atTag "div" -< x
        returnA -< History { note = note }
    )

filterIssueField field = hasAttrValue "id" (== "issue_" ++ field)

getIssueField field = deep (filterIssueField field  >>> getAttrValue "value")
getIssueFieldD field = deep (filterIssueField field  >>> getChildren >>> getText)

login :: Config -> IO ()
login config = curlGet (geturl config "login") []

geturl config path = (show $ url config) ++ path ++ "/"
