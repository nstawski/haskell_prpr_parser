{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.List.Split (splitOn)
import Types
import Parser
import Text.Megaparsec (parseTest, parse, eof, errorBundlePretty)
import System.Console.Haskeline
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import Control.Monad (unless, void)
import Data.Maybe (mapMaybe)
import Control.Monad.Trans.Except (Except)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as H

type Recipe = [(Component, Volume)]

data EvalError = UndefSymbolError String

type Env = H.HashMap String Value

-- Represent the empty environment

emptyEnv :: Env
emptyEnv = H.empty

-- To insert a value into an environment

insert :: a -> b -> [(a, b)] -> [(a, b)]
insert k v env = (k,v):env

eval :: ParsedLine -> Env -> (Value, Env)
eval (ComponentConfigLine v@(ComponentValue (Component name _ _))) env =
    (v, H.insert name v env)
eval (VolumeConfigLine v@(VolumeValue (Volume name _))) env = (v, H.insert name v env)
eval (PlateConfigLine v@(PlateValue (PlateConfig name _ _))) env = (v, H.insert name v env)

-- Eval for SPREAD command

eval (SpreadConfigLine a@(SpreadAction (Spread scomponent sdestination svolume smethod soptions))) env =
    let component = H.lookup scomponent env
        volume = H.lookup svolume env
        v = case (component, volume) of
              (Just (ComponentValue component), Just (VolumeValue volume)) -> 
                  SpreadValue (SpreadConfig component sdestination volume smethod soptions)
              _ -> error "Component or volume not found in environment"
    in (v, env)

-- Eval for TRANSFER command

eval (TransferConfigLine a@(TransferAction (Transfer tsource tdestination tvolume tmethod toptions))) env =
    let volume = H.lookup tvolume env
        v = case volume of
              (Just (VolumeValue volume)) -> 
                  TransferValue (TransferConfig tsource tdestination volume tmethod toptions)
              _ -> error "Volume not found in environment"
    in (v, env)

-- -- Symbol evaluates to the value bound to it
-- eval (Symbol sym) = do
--   env <- get
--   case lookup sym env of
--     Just val -> return val
--     Nothing  -> throwError $ UndefSymbolError sym

loop :: Show a => Parser a -> IO ()
loop parser = do
    line <- TIO.getLine -- now line is of type Text
    unless (T.toLower line == "quit") $ do
        case parse (parser <* eof) "" line of
            Left  e -> TIO.putStr (T.pack $ errorBundlePretty e)
            Right r -> print r
        loop parser

repl :: Env -> IO ()
repl env = do
    putStrLn "Enter 'quit' to exit, 'file' to parse a file, or a configuration line:"
    loop env
  where
    loop env = do
      putStr "config> "
      hFlush stdout
      line <- TIO.getLine
      case parse (lineParser <* eof) "" line of
        Right result -> do
          let (val, newEnv) = eval result env
          print val
          print newEnv
          loop newEnv
        Left err -> TIO.putStrLn (T.pack $ errorBundlePretty err)
      loop env
        -- ["quit"] -> exitSuccess
        -- ["file", filename] -> do
        --   fileContent <- TIO.readFile (T.unpack filename)
        --   case parse (config <* eof) (T.unpack filename) fileContent of
        --     Right result -> print result
        --     Left err -> TIO.putStrLn (T.pack $ errorBundlePretty err)
        -- _ -> do
        --   case parse (lineParser <* eof) "" line of
        --     Right result -> print result
        --     Left err -> TIO.putStrLn (T.pack $ errorBundlePretty err)
        --   loop

main :: IO ()
main = repl emptyEnv