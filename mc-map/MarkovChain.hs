{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main where

import qualified Data.Map as M
import System.Random (randomRIO)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.List
import System.Environment

type Suffix = T.Text

data Prefix = Prefix {prefix :: T.Text, chance :: Int}

type Storage = M.Map Suffix [Prefix]


instance  Show Prefix where
  show (Prefix pre chance) = "Prefix: " ++ (T.unpack pre) ++ " || Seen: " ++ show chance ++ "\n"


updatePrefix :: T.Text -> [Prefix] -> [Prefix]
updatePrefix p []                 = [(Prefix p 1)]
updatePrefix p ((Prefix p' c):xs)
  | p == p'   = (Prefix p' (c+1)) : xs
  | otherwise = (Prefix p'     c) : updatePrefix p xs


updatePsWith :: [T.Text] -> [Prefix] -> [Prefix]
updatePsWith []     xs = xs
updatePsWith (p:ps) xs = updatePsWith ps $ updatePrefix p xs

writeTable :: [T.Text] -> Storage -> Storage
writeTable (p1:p2:p3:pref:suf:xs) storage =
    let fullPref = p1 `T.append` " " `T.append` p2 `T.append` " " `T.append` p3 `T.append` " " `T.append` pref in
    case M.lookup suf storage of
        (Just _) -> let storage' = M.adjust (updatePsWith [fullPref]) suf storage in
                        writeTable (p2:p3:pref:suf:xs) storage'
        Nothing  -> let storage' = M.insert suf [(Prefix fullPref 1)] storage in
                        writeTable (p2:p3:pref:suf:xs) storage'
writeTable _          storage = storage

randomPrefix :: Int -> Int -> [Prefix] -> Prefix
randomPrefix _ _ [] = (Prefix "ERROR" 1)
randomPrefix n m ((Prefix p j):xs)
    | n <= m = Prefix p m
    | otherwise = randomPrefix n (m+j) xs

getSemiRandomPrefix :: [Prefix] -> IO(Prefix)
getSemiRandomPrefix list = do
  let maxR = foldl' (\xs (Prefix _ n) -> n + xs) 0 list
  r <- randomRIO (0, maxR - 1)
  return $ randomPrefix r 0 list

crtSentence :: Int -> T.Text ->  Storage -> T.Text -> IO()
crtSentence 0 _ _       acc = TIO.putStrLn acc
crtSentence c k storage acc = do
    let pref = if T.null k then "" else head $ T.words k
    case M.lookup pref storage of
        (Just prefixL) -> do
            (Prefix p _) <- getSemiRandomPrefix prefixL
            crtSentence (c-1) p storage (p `T.append` (' ' `T.cons` acc))
        Nothing        -> do
            index <- randomRIO (0, (M.size storage)- 1)
            let (k, prefixL) = M.elemAt index storage
            (Prefix p _) <- getSemiRandomPrefix prefixL
            crtSentence (c-1) p storage (p `T.append` (' ' `T.cons` acc))

doIt :: Int -> Storage -> IO()
doIt c table = crtSentence c "" table ""

createMap :: [FilePath] -> Storage -> IO(Storage)
createMap []      storage = return storage
createMap (fp:xs) storage = do
  f <- TIO.readFile fp
  let toFilter = "01234567889,!§$%&/()=?¹²³¼½¬{[]}'+*~'#’_:;>–…·|-<"
      parsedContent = T.map (\x -> if x `elem` toFilter then ' ' else x) f
  createMap xs (writeTable (T.words parsedContent) storage)


main :: IO()
main = do
    args <- getArgs
    case args of
      (x:fp:xs) | [(n,[])] <- reads x -> do
                  t <- createMap (fp:xs) M.empty
                  doIt n t
      _                 -> do
          putStrLn "Error in usage"