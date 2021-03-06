{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Config

import Data.Maybe (fromJust)
import Data.Either
import qualified Data.Map as Map
import Control.Monad.Trans (liftIO)

import Network.Curl
import Network.Curl.Opts
import Network.URI (URI, parseURI)

import System.Directory

import Text.HTML.TagSoup (parseTags)
import Text.XML.HXT.Core
import Text.XML.HXT.TagSoup (withTagSoup)

import Data.Tree
import Test.HUnit

data Config = Config { 
    username :: String,
    password :: String,
    url :: URI
} deriving Show

loadConfig m = fromJust $ do
    username <- lookup "username"
    password <- lookup "password"
    url <- lookup "url"
    Just Config {
        username = username, 
        password = password,
        url = fromJust $ parseURI url
    }
    where lookup s = Map.lookup s m

main = do
    dir <- getHomeDirectory
    conf <- readConf $ dir ++ "/.red"
    curl <- initialize
    setopts curl [CurlFollowLocation True]
    setopts curl [CurlCookieFile "cookies"]
    let config = either (error . show) loadConfig conf
    login curl config
    showIssue curl config "1921"

parse = readDocument parseOptions

parseOptions = [ 
        withParseHTML yes, 
        withWarnings no, 
        withTagSoup 
    ]

data Issue = Issue { subject :: String, description :: String, history :: [History] }
    deriving (Show, Eq)

data History = History { note :: String } deriving (Show, Eq)

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

login curl config = do
    r <- do_curl_ curl loginPageURL [] :: IO CurlResponse
    token <- getAuthToken (respBody r)
    r <- do_curl_ curl loginPageURL (method_POST ++ [CurlPostFields [ 
        set "username" username, 
        set "password" password, 
        "authenticity_token=" ++ (head token)
        ]]) :: IO CurlResponse
    errorMsg <- getLoginError $ respBody r
    if errorMsg == [] then return () else error (head errorMsg)
    where  
        set field cfield = field ++ "=" ++ (cfield config)
        loginPageURL = (appendURL config "login")
        getAuthToken page = runX $ readString parseOptions page >>>
            deep (hasAttrValue "name" (== "authenticity_token") >>> getAttrValue "value")
        getLoginError page = runX $ readString parseOptions page >>>
            deep (
                hasAttrValue "class" (== "flash error")
                >>> getChildren >>> getText
            )

showIssue curl config issue = do
    r <- do_curl_ curl showPageRL [] :: IO CurlResponse
    runX $ readString parseOptions (respBody r) >>> parseIssue
    where
        showPageRL = appendURL config ("issues/" ++ issue)

appendURL config path = (show $ url config) ++ "/" ++ path ++ "/"

-- Tests

tests = TestList [
        testParseIssue (Issue {
            subject = "JVM Custom Property Clean up - EDOI",
            description = "Part of the JVM Custom Property clean up\n\nedoi.appserver.name - remove and instead derive \n\nedoi.database.product.type - remove and instead derive\n\nedoi.duplicate.key.error.code - remove entirely\n\nedoi.load.used.modules.only.enabled - remove entirely",
            history = [
                History {note = "When finished assign back to Matt O to remove from ControlTier"},
                History {note = "None of these are needed for PD190."}
            ]
        }) "test/issue1.html"
    ]

testParseIssue expected file = TestCase $ do
        issue <- runX $ parse file >>> parseIssue
        print issue
        assertEqual ("Test parsing " ++ file) expected (head issue)
