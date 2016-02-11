module Main where

import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import System.Process (rawSystem)

import Control.Monad (join)
import Data.List (nub, sort)
import Text.Parsec
import Text.Parsec.String


data PageSpec = P Integer | R Integer Integer deriving (Show)
data Topic = Topic String [PageSpec] [Topic] deriving (Show)

-- PARSERS
-- =======

-- | Parse the entire index of topics.
topicsP :: Parser [Topic]
topicsP = manyTill topicP (try $ spaces *> eof)

-- | Parse a top level topic and its subtopics.
topicP :: Parser Topic
topicP = topicP' <*> many subTopicP

-- | Partial parse of a topic.
topicP' :: Parser ([Topic] -> Topic)
topicP' = Topic <$> titleP <*> sepBy pageSpecP commaP <* many1 endOfLine

-- | Parse a first level sub topic.
subTopicP :: Parser Topic
subTopicP = char '-' *> topicP' <*> return []

titleP :: Parser String
titleP = spaces *> manyTill anyChar (try $ spaces <* char ':')

pageSpecP :: Parser PageSpec
pageSpecP = do
  page1 <- pageP
  try (R page1 <$> (spaces *> char '-' *> pageP)) <|> return (P page1)  -- TODO buggy, will not fail on e.g. "12-afa".

commaP :: Parser ()
commaP = try (spaces <* char ',')

-- | Parse a page number (natural number).
pageP :: Parser Integer
pageP = read <$> try (spaces *> many1 digit) :: Parser Integer



-- | Extract all pages pertaining to a topic, including those of subtopics.§
topicPages :: Topic -> [Integer]
topicPages (Topic title pages subs) = nub $ sort $ concatMap massage pages ++ concatMap topicPages subs
  where
    massage :: PageSpec -> [Integer]
    massage (P p) = return p
    massage (R p1 p2) = [p1 .. p2]

-- | Reduce a topic and its subtopics to a list of pages and title pairs.
flatten :: Topic -> [([Integer], String)]
flatten topic@(Topic title pages subs) = massage topic : map (fmap prefix . massage) subs
  where
    massage :: Topic -> ([Integer], String)
    massage topic@(Topic title _ _) = (topicPages topic, title)
    prefix subtitle = title ++ " - " ++ subtitle

main :: IO ()
main = do
  [indexFile, sourcePDF] <- getArgs
  res <- parseFromFile topicsP indexFile
  case res of
    Right index -> mapM_ (makePDF sourcePDF) $ concatMap flatten index
    Left err -> print err

makePDF sourcePDF (pages, title) = do
  print $ title ++ ": " ++ show pages
  code <- rawSystem "pdftk" $ [sourcePDF, "cat"] ++ map show pages ++ ["output", "out" </> title <.> "pdf"]
  case code of
    exitSuccess -> return ()
    otherwise   -> error $ "Failed on topic " ++ title
