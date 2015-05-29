{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text (unpack, append, Text)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

import System.IO
import System.Environment (getArgs, getProgName)
import Data.Char (isNumber)


import ChanTypes

--getJSON :: IO B.ByteString
--getJSON = B.readFile "post.json"


getPageThreads :: Int -> [Page] -> [Integer]
getPageThreads n []     = error $ "Some error happend while fetching on " ++ show n ++ " page :/"
getPageThreads 1 (x:xs) = map no $ threads x
getPageThreads n (x:xs) = getPageThreads (n-1) xs



getThreadByID :: String  -- board
              -> Integer -- threadnumber
              -> IO (Thread)
getThreadByID board ident = do
    res <- simpleHttp ("http://a.4cdn.org/" ++ board ++ "/thread/" ++ show ident ++ ".json")
    case eitherDecode res :: Either String Thread of
      Left err     -> return (Thread [Post ", "])
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


allBoards :: [String]
allBoards = ["a", "b", "c", "d", "e", "f", "g", "gif", "h", "hr", "k", "m", "o", "p", "r", "s", "t", "u", "v",
             "vg", "vr", "w", "wg", "i", "ic", "r9k", "s4s", "cm", "hm", "lgbt", "y", "3", "adv", "an", "asp",
             "biz", "cgl", "ck", "co", "diy", "fa", "fit", "gd", "hc", "int", "jp", "lit", "mlp", "mu", "n",
             "out", "po", "pol", "sci", "soc", "sp", "tg", "toy", "trv", "tv", "vp", "wsg", "x"]


jsonAction :: String   -- board to fetch from (f.e "g")
           -> FilePath -- path to .txt-file
           -> IO ()
jsonAction board fp = do

    hSetBuffering stdout NoBuffering

    let validBoard = board `elem` allBoards
    res <- (eitherDecode <$> simpleHttp ("http://a.4cdn.org/" ++ board ++ "/threads.json")) :: IO (Either String [Page])

    case (res, validBoard) of
        (_, False)          -> putStrLn "Sorry, this board can't be found..."
        (Left err,_)        -> putStrLn err
        (Right pages, True) -> do

            let threadIds = concatMap (\x -> getPageThreads x pages) [1..10]
            putStrLn "Fetched current information about threads..."
            threads <- forM threadIds (getThreadByID board)
            putStrLn "Done extracting posts from all threads..."
            let !lines = threadsToLines threads
            putStrLn "Done converting threads to lines.."
            let !filteredInput = filterTags lines
            putStrLn "Done filtering tabs..."
            let content = concatMap (\x -> x ++ "\n") filteredInput
            writeFile fp content
            putStrLn $ "Content stored in " ++ fp ++ "!"


main :: IO()
main = do
    args <- getArgs
    case args of
      ["-board", board, "-to", to] -> jsonAction board to
      _            -> do
        name <- getProgName
        putStrLn "Usage:"
        putStrLn $ "./" ++ name ++ " -board <4chanboard> -to <filepath to .txt>"

