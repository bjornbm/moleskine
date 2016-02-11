module Main where

import Lib
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import System.Process (rawSystem)
--import System.IO

import Control.Monad (join)
import Data.List (nub, sort)
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

data PageSpec = P Integer | R Integer Integer deriving (Show)
data Topic = Topic String [PageSpec] [Topic] deriving (Show)

topicsP = manyTill topicP (spaces *> eof) :: Parser [Topic]
topicP = topicP' <*> many subTopicP :: Parser Topic
topicP' = Topic <$> titleP <*> sepBy pageSpecP comma <* many1 endOfLine :: Parser ([Topic] -> Topic)

subTopicP = char '-' *> spaces *> topicP' <*> return [] :: Parser Topic

titleP = manyTill anyChar (try (spaces <* char ':')) :: Parser String
pageSpecP :: Parser PageSpec
pageSpecP = do
  page1 <- pageP
  try (R page1 <$> (spaces *> char '-' *> pageP)) <|> return (P page1)

comma = try (spaces <* char ',')
pageP = read <$> (try (spaces *> many1 digit)) :: Parser Integer


topicPages :: Topic -> [Integer]
topicPages (Topic title pages subs) = nub $ sort $ concatMap massage pages ++ concatMap topicPages subs
  where
    massage :: PageSpec -> [Integer]
    massage (P p) = return p
    massage (R p1 p2) = [p1 .. p2]

flatten :: Topic -> [([Integer], String)]
flatten topic@(Topic title pages subs) = massage topic : map (fmap (\st -> title ++ " - " ++ st) . massage) subs
  where
    massage :: Topic -> ([Integer], String)
    massage topic@(Topic title _ _) = (topicPages topic, title)

main :: IO ()
main = do
  [indexFile, pdfFile] <- getArgs
  res <- parseFromFile topicsP indexFile
  case res of
    Right index -> mapM_ (writeTopic pdfFile) $ concatMap flatten index
    Left err -> print err

writeTopic pdfFile (pages, title) = do
  print $ title ++ ": " ++ show pages
  code <- rawSystem "pdftk" $ [pdfFile, "cat"] ++ map show pages ++ ["output", "out" </> title <.> "pdf"]
  case code of
    exitSuccess -> return ()
    otherwise   -> error $ "Failed on topic " ++ title
