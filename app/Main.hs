module Main where

import Lib
import System.Environment (getArgs)
import System.Process (rawSystem)
--import System.IO

import Control.Monad (join)
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)


type Page = Integer
data Topic = Topic String [Page] deriving (Show)

topicsP = manyTill topicP (spaces *> eof) :: Parser [Topic]
topicP = Topic <$> titleP <*> pagesP <* endOfLine :: Parser Topic
titleP = manyTill anyChar (try (spaces <* char ':')) :: Parser String
pagesP = join <$> (sepBy (try rangeP <|> (return <$> pageP)) (try (spaces <* char ','))) :: Parser [Page]
pageP = read <$> (try (spaces *> many1 digit)) :: Parser Page
rangeP = enumFromTo <$> (pageP <* spaces <* char '-') <*> pageP :: Parser [Page]


main :: IO ()
main = do
  [indexFile, pdfFile] <- getArgs
  Right index <- parseFromFile topicsP indexFile
  mapM_ (writeTopic pdfFile) index

writeTopic :: String -> Topic -> IO ()
writeTopic pdfFile (Topic title pages) = do
  code <- rawSystem "pdftk" $ [pdfFile, "cat"] ++ map show pages ++ ["output", title ++ ".pdf"]
  case code of
    exitSuccess -> return ()
    otherwise   -> error $ "Failed on topic " ++ title
