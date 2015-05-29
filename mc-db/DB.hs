{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

import qualified Data.Conduit.List as CL
import Data.Conduit (($$))
import Control.Monad.IO.Class (liftIO, MonadIO())
import Control.Monad.Trans.Reader (ReaderT())
import Database.Persist.Sqlite
import Database.Persist.TH

import Prefix
import Data.Text (pack, Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TL

import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import System.Random (randomRIO)
import Data.List (foldl')

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Word
    suffix T.Text
    UniqueSuffix suffix
    prefixes [Prefix]
    deriving Show
|]

-- #################### LEARNING ####################

-- | [T.Text] -> DB Action
--   Parse a list of lazy Text and matches four words
--   The first three are the prefix, the last one is the suffix
--   Updating the DB with the UniqueSuffix and adding
--   the prefix to the list

parseWordsToDB (pref:pref':pref'':pref''':suf:xs) = do
    mword <- getBy $ UniqueSuffix suf
    let prefix = pref `T.append` " " `T.append` pref' `T.append` " " `T.append` pref'' `T.append` " " `T.append` pref'''
    case mword of
      Nothing                          -> do
          insert $ Word suf [(Prefix prefix 1)]
          parseWordsToDB (pref':pref'':pref''':suf:xs)
      Just (Entity id (Word _ prefL)) -> do
          let newPrefL = updatePrefL prefL (Prefix prefix 1)
          update id [WordPrefixes =. newPrefL]
          parseWordsToDB (pref':pref'':pref''':suf:xs)
parseWordsToDB _        = return ()


-- | [FilePath] -> DB Action
--   Reads the files with Text.Lazy
--   Updates the DB with all the input

parseFiles []     = liftIO $ print "Done parsing!"
parseFiles (filename:xs) = do
  exists  <- liftIO $ doesFileExist filename
  case exists of
    False -> do
        liftIO $ print $ "Sorry, " ++ filename ++ " can't be found."
        parseFiles xs
    True  -> do
        content <- liftIO $ TL.readFile filename
        parseWordsToDB $ T.words content
        liftIO $ print $ "Parsed file " ++ filename ++ "!..."
        parseFiles xs


updatePrefL :: [Prefix] -> Prefix -> [Prefix]
updatePrefL []                pre  = [pre]
updatePrefL (p1@(Prefix w c):xs) p2@(Prefix w' c')
    | w == w'   = (Prefix w (c+c')) : xs
    | otherwise = p1 : updatePrefL xs p2



-- | Single Connection with DB
--   All learning access to DB is called within this function

learnAction :: Text -> [FilePath] -> IO()
learnAction fp fps = runSqlite fp $ do
    runMigration migrateAll

    parseFiles fps

    return ()

-- #################### CREATE SENTENCE ####################


-- | Int -> DB Action
--   Ques the DB and constructs a sentence with N words
-- getSentence n = do
--     list <- selectList [] [Asc WordSuffix]
--     sentence <- liftIO $ createSentence list Nothing n
--     liftIO $ print $ T.unwords sentence


-- createSentence :: [Entity Word] -> Maybe Word -> Int -> IO ([T.Text])
-- createSentence _            _             0   = return []
-- createSentence list         Nothing       n   = do
--     r <- randomRIO (0, (length list) - 1)
--     let (Entity _ word) = list !! r
--     createSentence list (Just word) n

-- createSentence list (Just (Word _ prefL)) n = do
--     let prefLLength = foldl' (\xs (Prefix _ m) -> m + xs) 0 prefL
--     r' <- randomRIO (0, prefLLength - 1)
--     let longPrefix  = getSemiRandomThunk prefL r'
--         newSuf      = (T.words longPrefix) !! 0
--         mword       = lookupSuf list newSuf
--     rest <- createSentence list mword (n-1)
--     return $ rest ++ [longPrefix]

-- lookupSuf :: [Entity Word] -> T.Text -> Maybe Word
-- lookupSuf [] _ = Nothing
-- lookupSuf (Entity _ (word@(Word w _)):xs) w'
--   | w == w'   = Just word
--   | otherwise = lookupSuf xs w'


getSemiRandomThunk :: [Prefix] -> Int -> T.Text
getSemiRandomThunk []     _ = "!!!ERROR!!!"
getSemiRandomThunk ((Prefix w n):xs) c
    | c - n <= 0 = w --(T.words w) !! 0
    | otherwise  = getSemiRandomThunk xs (c-n)


stripEntity :: Maybe (Entity Word) -> Maybe Word
stripEntity (Just (Entity _ word)) = Just word
stripEntity Nothing                = Nothing


-- Ques in the DB and constructs a sentence with N thunks
constructSentence :: (Control.Monad.IO.Class.MonadIO m)
                  => Integer     -- DB size
                  -> Int         -- thunks
                  -> Maybe Word
                  -> ReaderT SqlBackend m T.Text

-- end condition
constructSentence _ 0 mword = return $ ""

-- no suffix was found
constructSentence dbSize thunks Nothing = do
    r <- liftIO $ randomRIO (0, dbSize)
    mword <- get $ (toSqlKey (fromIntegral r) :: Key Word)
    rest <- constructSentence dbSize thunks mword
    return $ rest

-- word doesn't have any prefixes (shouldn't happen)
constructSentence dbSize thunks (Just (Word suf [])) = do
    r <- liftIO $ randomRIO (0, dbSize)
    mword <- get $ (toSqlKey (fromIntegral r) :: Key Word)
    rest <- constructSentence dbSize (thunks - 1) mword
    return $ rest `T.append` " " `T.append` suf

-- word does have prefixes -> choose a semi-random one
constructSentence dbSize thunks (Just (Word suf ps)) = do
    let psmax = foldl' (\acc (Prefix _ val) -> val + acc) 0 ps
    r <- liftIO $ randomRIO (0, psmax)
    let prefix = getSemiRandomThunk ps r
    let newsuffix = T.words prefix !! 0
    mword <- getBy $ UniqueSuffix newsuffix
    rest <- constructSentence dbSize (thunks - 1) (stripEntity mword)
    return $ rest `T.append` " " `T.append` prefix


-- | Single Connection with DB
--   Every 'GET' access to DB is called within this function
getAction :: Text -> Int -> IO()
getAction fp n = runSqlite fp $ do
    runMigration migrateAll

    let sql = "Select Count(*) FROM WORD;"
    [[PersistInt64 i]] <- rawQuery sql [] $$ CL.consume
    let dbRowCount = fromIntegral i :: Integer

    sentence <- constructSentence dbRowCount n Nothing

    liftIO $ print $ T.drop 1 sentence

    return ()

-- | Main entry point
--   Simple command line support
main :: IO()
main = do
    args <- getArgs
    hSetBuffering stdout NoBuffering
    case args of
      ("-db" : db : "-learn" : xs)                        -> learnAction (pack db) xs
      ["-db", db, "-get", x] | [(n :: Int,[])] <- reads x -> getAction (pack db) n
      _                                                   -> do
          pname <- getProgName
          putStrLn "How to use:"
          putStrLn $ pname ++ " -db <database-path> -learn <txt-file-path(s)>"
          putStrLn $ pname ++ " -db <database-path> -get <thunk-size in int>"

