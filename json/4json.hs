{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text (unpack, append, Text)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

import System.IO
import System.Environment (getArgs)
import Data.Char (isNumber)


import ChanTypes

--getJSON :: IO B.ByteString
--getJSON = B.readFile "post.json"

-- Read the remote copy of the JSON file.
getActivePages :: IO (B.ByteString)
getActivePages = simpleHttp "http://a.4cdn.org/g/threads.json"

getPageThreads :: Int -> [Page] -> [Integer]
getPageThreads n []     = error $ "Some error happend while fetching on " ++ show n ++ " page :/"
getPageThreads 1 (x:xs) = map no $ threads x
getPageThreads n (x:xs) = getPageThreads (n-1) xs



getThreadByID :: Integer -> IO (Thread)
getThreadByID ident = do
    res <- simpleHttp ("http://a.4cdn.org/g/thread/" ++ show ident ++ ".json")
    case eitherDecode res :: Either String Thread of
      Left err     -> return (Thread [Post ""])
      Right thread -> return thread

filterTags :: [String] -> [String]
filterTags text = map (filterTags' False) text
  where filterTags' :: Bool -> String -> String
        filterTags' _     []          = []
        filterTags' _     ('<':xs)    = filterTags' True xs
        filterTags' _     ('>':xs)    = filterTags' False xs
        filterTags' True  (x:xs)      = filterTags' True xs
        filterTags' False ('&':'g':'t':';':'&':'g':'t':';':xs) = filterTags' False (drop8Nums xs)
        filterTags' False ('&':'a':'m':'p':';': xs) = ['&'] ++ filterTags' False xs
        filterTags' False ('&':'g':'t':';': xs) = filterTags' False xs
        filterTags' False ('&':'l':'t':';': xs) = filterTags' False xs
        filterTags' False ('(': xs) = filterTags' False xs
        filterTags' False (')': xs) = filterTags' False xs
        filterTags' False ('&':'#':'0':'3':'9': ';': xs) = ['\''] ++ filterTags' False xs
        filterTags' False ('&':'q':'u':'o':'t': ';': xs) = ['"'] ++ filterTags' False xs
        filterTags' False (x:xs)      = [x] ++ filterTags' False xs


drop8Nums :: String -> String
drop8Nums (a:b:c:d:e:f:g:h:xs)
    | all isNumber [a,b,c,d,e,f,g,h] = xs
drop8Nums xs = xs

threadsToLines :: [Thread] -> [String]
threadsToLines []               = []
threadsToLines ((Thread l):xs) = postsToLines l ++ threadsToLines xs
  where postsToLines :: [Post] -> [String]
        postsToLines []     = []
        postsToLines ((Post y):ys) = [unpack y] ++ postsToLines ys

getThreadIds :: Bool -> [Integer] -> [Page] -> [Integer]
getThreadIds True l pages = l ++ concatMap (\x -> getPageThreads x pages) [1..10]
getThreadIds _    _ pages = concatMap (\x -> getPageThreads x pages) [1..10]

--        with Archive
jsonAction :: Bool -> FilePath -> IO ()
jsonAction archive fp = do

    res <- (eitherDecode <$> getActivePages) :: IO (Either String [Page])
    res' <- (eitherDecode <$> simpleHttp "http://a.4cdn.org/g/archive.json") :: IO (Either String [Integer])

    case (res, res') of
        (Left err, _)    -> putStrLn err
        (Right pages, Right ids) -> do
          hSetBuffering stdout NoBuffering
          let threadIds = getThreadIds archive ids pages
          putStrLn "Fetched current information about threads..."
          threads <- forM threadIds getThreadByID
          putStrLn "Done extracting posts from all threads..."
          let !lines = threadsToLines threads
          putStrLn "Done converting threads to lines.."
          let !filteredInput = filterTags lines
          putStrLn "Done filtering tabs..."
          let content = concatMap (\x -> x ++ "\n") filteredInput
          writeFile fp content
          putStrLn $ "Content storen in " ++ fp ++ "!"


main :: IO()
main = do
    args <- getArgs
    case args of
      --("-to":fp:"-archive":_) -> jsonAction True fp
      ("-to":fp:_)            -> jsonAction False fp
      _                     -> putStrLn "Usage: ./gchan -to <filename> <optional -archive to include all threads>"
