{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Types

import Data.Void
import Data.Text (Text, pack, unpack)
import Data.Maybe (fromMaybe)
import Text.Megaparsec 
import Text.Megaparsec.Char
import Data.Char (ord)
import Text.Megaparsec.Char.Lexer (decimal)
import Distribution.Compat.CharParsing (spaces)

type Parser = Parsec Void Text

comment :: Parser ()
comment = do
  _ <- char '#' <* manyTill anySingle newline
  pure ()

plateConfig :: Parser Value
plateConfig = do
  _ <- string "PLATE"
  some spaceChar
  plateName <- manyTill letterChar spaceChar 
  some spaceChar
  prefix <- string "PL"
  num <- decimal
  let plateId = (unpack prefix) ++ show num  -- convert Text to String and concatenate
  let pCapacity = plateCapacity plateId
  let newPlate = PlateValue (PlateConfig plateName plateId pCapacity )
  return newPlate where
    plateCapacity x 
        | x == "PL4" = (2,4)
        | x == "PL6" = (3,6)
        | x == "PL8" = (4,8)
        | otherwise  = (1,1) -- or raise error?

locationChars = satisfy (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ":,+") 

componentConfig :: Parser Value
componentConfig = do
    _ <- string "COMPONENT"
    some spaceChar
    componentName <- manyTill letterChar spaceChar 
    some spaceChar
    unparsedLocation <- manyTill locationChars spaceChar
    componentLocation <- parseFromString location (pack unparsedLocation)
    some spaceChar
    unparsedMethod <- manyTill anySingle eof
    componentMethod <- parseFromString method (pack unparsedMethod)
    let newComponent = ComponentValue (Component componentName componentLocation componentMethod)
    return newComponent

volumeConfig :: Parser Value
volumeConfig = do
    _ <- string "VOLUME"
    some spaceChar
    volumeName <- manyTill letterChar spaceChar
    some spaceChar
    volumeValue <- decimal
    let newVolume = VolumeValue (Volume volumeName volumeValue)
    return newVolume

well :: Parser (Char, [Int])
well = do
    row <- letterChar
    startCol <- decimal
    extra <- optional $ char '+' >> decimal
    let cols = case extra of
                    Just n  -> [startCol .. startCol + n - 1]
                    Nothing -> [startCol]
    return (row, cols)

location :: Parser Location
location = do
    plate <- manyTill anySingle (char ':')
    wellGroups <- wellGroup `sepBy1` char ','
    let wells = [(row, col) | (row, cols) <- wellGroups, col <- cols]
    return $ Location plate wells
    where
        wellGroup = do
            row <- optional (satisfy (\c -> c >= 'A' && c <= 'Z'))  -- only uppercase letters
            start <- decimal
            len <- optional (char '+' *> decimal)
            case row of
              Just r -> return (r, [start .. start + fromMaybe 1 len - 1]) -- for the case with row and range
              Nothing -> return ('A', [start]) -- for the case with a single well number

method :: Parser TransferMethod
method = (LC_W_Bot_Bot <$ string "DEFAULT")
     <|> (LC_W_Bot_Bot <$ string "LC_W_Bot_Bot")
     <|> (LC_W_Bot_Lev <$ string "LC_W_Bot_Lev")
     <|> (LC_W_Bot_Air <$ string "LC_W_Bot_Air")
     <|> (LC_W_Lev_Bot <$ string "LC_W_Lev_Bot")
     <|> (LC_W_Lev_Lev <$ string "LC_W_Lev_Lev")
     <|> (LC_W_Lev_Air <$ string "LC_W_Lev_Air")

spreadConfig :: Parser Action
spreadConfig = do
    _ <- string "SPREAD"
    some spaceChar
    component <- manyTill letterChar spaceChar
    some spaceChar
    destination <- location
    some spaceChar
    volumeName <- manyTill letterChar spaceChar
    some spaceChar
    methodValue <- method
    some spaceChar
    optionsValue <- manyTill anySingle eof
    let newSpread = SpreadAction (Spread component destination volumeName methodValue optionsValue)
    return newSpread

transferConfig :: Parser Action
transferConfig = do
    _ <- string "TRANSFER"
    some spaceChar
    source <- location
    some spaceChar
    destination <- location
    some spaceChar
    volumeName <- manyTill anySingle spaceChar
    some spaceChar
    methodValue <- method
    some spaceChar
    optionsValue <- manyTill anySingle eof
    let newSpread = TransferAction (Transfer source destination volumeName methodValue optionsValue)
    return newSpread

-- humanComment :: Parser Comment
-- humanComment = do
--   _ <- char '#'
--   some spaceChar
--   comment <- manyTill anySingle newline
--   return HumanComment comment

-- robotMessage :: Parser Comment
-- robotMessage = do
--   _ <- char '%'
--   some spaceChar
--   message <- manyTill anySingle newline
--   return RobotMessage message

-- ioMessage :: Parser Comment
-- ioMessage = do
--   _ <- string "MESSAGE"
--   some spaceChar
--   message <- manyTill anySingle newline
--   return IOMessage message

lineParser :: Parser ParsedLine
lineParser = choice [ try $ Method <$> method
    -- , try $ PlateConfigLine <$> plateConfig
    , try $ PlateConfigLine <$> plateConfig
    , try $ ComponentConfigLine <$> componentConfig
    , try $ VolumeConfigLine <$> volumeConfig
    , try $ SpreadConfigLine <$> spreadConfig
    , try $ TransferConfigLine <$> transferConfig
    -- , try $ HumanCommentLine <$> humanComment
    -- , try $ RobotMessageLine <$> robotMessage
    -- , try $ IOMessageLine <$> ioMessage
    ]

parseFromString :: Parser a -> Text -> Parser a
parseFromString p str = case runParser p "" str of
    Left _  -> empty
    Right a -> return a