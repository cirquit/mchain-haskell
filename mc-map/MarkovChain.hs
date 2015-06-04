{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables, BangPatterns #-}

module Main where

import qualified Data.Map as M
import System.Random (randomRIO)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.List
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Exit (exitSuccess)

import Control.Monad (forever)

type Suffix = T.Text

data Prefix = Prefix {prefix :: T.Text, chance :: Int}

type Storage = M.Map Suffix [Prefix]


------------------------------------------------------------------------------------
--------------------------------- GET SENTENCE -------------------------------------
------------------------------------------------------------------------------------

-- | Prints a sentence with N thunks

--   Searches the Map for a suffix
--   If it exists, get a semi-random prefix and use
--   the first word as a new key to search the map
--   until all thunks are generated
printSentence :: Int -> Storage -> IO()
printSentence thunkscount table = printS thunkscount "" table ""
  where printS :: Int     -- thunk count
               -> T.Text  -- key
               -> Storage
               -> T.Text  -- accumulated thunks
               -> IO()

        printS 0 _ _       acc = TIO.putStrLn $ "\n..." `T.append` acc `T.append` "..."

        printS thunks key storage acc = do
            case M.lookup key storage of
                (Just prefixL) -> do

                    (Prefix prefix _) <- getSemiRandomPrefix prefixL
                    let key' | T.null prefix  = ""
                             | otherwise = head $ T.words prefix
                    printS (thunks-1) key' storage (T.unwords [prefix, acc])

                Nothing        -> do

                    index <- randomRIO (0, (M.size storage)- 1)
                    let (_, prefixL) = M.elemAt index storage
                    (Prefix prefix _) <- getSemiRandomPrefix prefixL
                    let key | T.null prefix  = ""
                            | otherwise = head $ T.words prefix
                    printS (thunks-1) key storage (T.unwords [prefix, acc])


-- | get a random Prefix from a [Prefix] based
--   while considering the occurencies from each Prefix
getSemiRandomPrefix :: [Prefix] -> IO (Prefix)
getSemiRandomPrefix list = do
  let maxR = foldl' (\xs (Prefix _ n) -> n + xs) 0 list
  r <- randomRIO (0, maxR - 1)
  return $ getPrefixAt r list

getPrefixAt :: Int -> [Prefix] -> Prefix
getPrefixAt _ [] = error "random index out of bounds"
getPrefixAt n ((Prefix p j):xs)
    | n - j <= 0 = Prefix p j
    | otherwise  = getPrefixAt (n-j) xs


------------------------------------------------------------------------------------
--------------------------------- LEARNING -----------------------------------------
------------------------------------------------------------------------------------

-- | create a Map from every file
--   if it doesn't exist, proceed to the next file
createMap :: [FilePath] -> Storage -> IO (Storage)
createMap []            storage = return storage
createMap (filename:xs) storage = do
    exists  <- doesFileExist filename
    case exists of
        False -> do
            print $ "Sorry, " ++ filename ++ " can't be found."
            createMap xs storage
        True  -> do
            content <- TIO.readFile filename
            let !storage' = foldl' (\xs x -> writeTable (T.words x) xs) storage (T.lines content)
            print $ "Parsed file " ++ filename ++ "!..."
            createMap xs storage'

-- | matches 5 words
--   first 4 are the prefixs, the fifth one is the suffix
--   update the Map with Suffix as key and [Prefix] as values
writeTable :: [T.Text] -> Storage -> Storage
writeTable (p1:p2:p3:p4:suf:xs) storage =
    let prefix = T.unwords [p1,p2,p3,p4] in
    case M.lookup suf storage of
        (Just _) -> let storage' = M.adjust (updatePrefixList prefix) suf storage in
                        writeTable (p2:p3:p4:suf:xs) storage'
        Nothing  -> let storage' = M.insert suf [(Prefix prefix 1)]       storage in
                        writeTable (p2:p3:p4:suf:xs) storage'
writeTable _             storage = storage

-- | Increments counter if the prefix exists
--   If not, it appends the new prefix with count = 1
updatePrefixList :: T.Text -> [Prefix] -> [Prefix]
updatePrefixList prefix []                = [(Prefix prefix 1)]
updatePrefixList prefix ((Prefix p c)     : xs)
    | prefix == p      = (Prefix p (c+1)) : xs
    | otherwise        = (Prefix p c)     : updatePrefixList prefix xs


------------------------------------------------------------------------------------
------------------------------ MAIN ENTRY POINT ------------------------------------
------------------------------------------------------------------------------------

-- |  Short command line support
main :: IO()
main = do
    args <- getArgs
    hSetBuffering stdout NoBuffering
    case args of
      ("-get":x:"-from":xs) | [(thunks :: Int,[])] <- reads x -> do
          suf_map <- createMap xs M.empty
          printSentence thunks suf_map
          loop suf_map
      _                 -> do
          pname <- getProgName
          putStrLn "How to use:"
          putStrLn $ pname ++ " -get <thunk-size in int> -from <txt-file-path(s)>"

loop :: Storage -> IO()
loop suf_map = do
    forever $ do
        putStr "\nThunkscount = "
        line <- getLine
        case line of
           x | [(thunks :: Int,[])] <- reads x -> printSentence thunks suf_map
           "q"                                 -> exitSuccess
           "Q"                                 -> exitSuccess
           ":q"                                -> exitSuccess
           ":Q"                                -> exitSuccess
           "quit"                              -> exitSuccess
           _                                   -> putStrLn "\nSorry, not a valid number"