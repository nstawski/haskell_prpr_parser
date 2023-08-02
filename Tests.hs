{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.QuickCheck
import Test.HUnit
import Parser
import Types
import Text.Megaparsec
import Data.Void (Void)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Control.Exception (evaluate)

assertEqual' :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual' msg expected actual =
  if expected == actual
    then putStrLn $ "Pass: " ++ msg
    else assertFailure $ "Fail: " ++ msg ++ "\nExpected: " ++ show expected ++ "\nBut got:  " ++ show actual

prConfigCases :: [(T.Text, Either (ParseErrorBundle T.Text Void) ParsedLine)]
prConfigCases = [ ("PLATE     DrinksPlate  PL4", Right (PlateConfigLine (PlateValue (PlateConfig {plateName = "DrinksPlate", plateId = "PL4", plateCapacity = (2,4)}))))
                , ("VOLUME    DrinkVol     50", Right (VolumeConfigLine (VolumeValue (Volume {volumeName = "DrinkVol", volumeValue = 50}))))
                -- Component tests
                , ("COMPONENT Water        PL8:A1+4,F1  LC_W_Lev_Air", Right (ComponentConfigLine (ComponentValue (Component {componentName = "Water", componentLocation = Location {plate = "PL8", wells = [('A',1),('A',2),('A',3),('A',4),('F',1)]}, componentMethod = LC_W_Lev_Air}))))
                , ("COMPONENT TeaExtract   DrinksPlate:17       LC_W_Lev_Bot", Right (ComponentConfigLine (ComponentValue (Component {componentName = "TeaExtract", componentLocation = Location {plate = "DrinksPlate", wells = [('A',17)]}, componentMethod = LC_W_Lev_Bot}))))
                , ("COMPONENT Syrup        PL7:18       LC_W_Lev_Bot", Right (ComponentConfigLine (ComponentValue (Component {componentName = "Syrup", componentLocation = Location {plate = "PL7", wells = [('A',18)]}, componentMethod = LC_W_Lev_Bot}))))
                -- Templates for future use
                -- , ("", Right (ComponentConfigLine ()))
                -- , ("", Right (ComponentConfigLine ()))
                -- , ("", Right (ComponentConfigLine ()))
                -- , ("", Right (ComponentConfigLine ()))
                -- , ("", Right (ComponentConfigLine ()))
                ]

testPRConfig :: [Test]
testPRConfig = map makeTest (zip [1..] prConfigCases)
  where makeTest (i, (input, expected)) =
          TestCase $
            assertEqual' ("testPRConfig " ++ show i) expected (runParser lineParser "" input)

tests :: Test
tests = TestList ([TestLabel ("test" ++ show i) t | (i, t) <- zip [1..] testPRConfig]
                 )

main :: IO Counts
main = runTestTT tests