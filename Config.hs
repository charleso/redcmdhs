module Config where

import Char
import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import Text.ParserCombinators.Parsec hiding (parse)
import Test.HUnit

-- http://www.serpentine.com/blog/2007/01/31/parsing-a-simple-config-file-in-haskell/

ident = do 
        c <- letter <|> char '_'
        cs <- many (letter <|> digit <|> char '_')
        return (c:cs)
    <?> "identifier"

comment = do 
        char '#'
        skipMany (noneOf "\r\n")
    <?> "comment"

eol = do oneOf "\n\r"
         return ()
    <?> "end of line"

item = do key <- ident
          skipMany space
          char ':'
          skipMany space
          value <- manyTill anyChar (try eol <|> try comment <|> eof)
          return (key, rstrip value)
    where rstrip = reverse . dropWhile isSpace . reverse

line = do 
    skipMany space
    try (comment >> return Nothing) <|> (item >>= return . Just)

file = do lines <- many line
          return (catMaybes lines)

readConf =
    (return . fmap Map.fromList =<<) .
    parseFromFile file

tests = TestList [
        TestCase $ parseTest file "username: me\npassword: pass"
    ]
