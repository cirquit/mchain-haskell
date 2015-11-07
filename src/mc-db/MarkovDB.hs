{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}

import Data.Conduit.List (consume)
import Data.Conduit (($$))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT())
import Control.Monad.Logger (NoLoggingT())
import Control.Monad.Trans.Resource.Internal (ResourceT())
import Database.Persist.Sqlite
import Database.Persist.TH (persistLowerCase, mkMigrate, mkPersist, share, sqlSettings)

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

-- ####################               ####################
-- #################### LEARN REQUEST ####################
-- ####################               ####################

-- | Parse a list of lazy Text and matches four words
--   The first three are the prefix, the last one is the suffix
--   Updating the DB with the UniqueSuffix and adding
--   the prefix to the list

parseWordsToDB :: [T.Text] -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))  ()
parseWordsToDB (p1:p2:p3:suf:xs) = do
    mword <- getBy $ UniqueSuffix suf
    let prefix = T.unwords [p1, p2, p3]
    case mword of
      Nothing                          -> do
          insert $ Word suf [(Prefix prefix 1)]
          parseWordsToDB (p2:p3:suf:xs)
      Just (Entity id (Word _ prefL)) -> do
          let newPrefL = updatePrefixList prefL (Prefix prefix 1)
          update id [WordPrefixes =. newPrefL]
          parseWordsToDB (p2:p3:suf:xs)
parseWordsToDB _        = return ()


-- | Reads the files with Text.Lazy
--   Updates the DB with all the input
--   Seperates every post as a line an tries to match four suffixes for every prefix
--   Post are not going to be appended to each other, because it creates wrong output

parseFiles :: [FilePath] -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
parseFiles []     = liftIO $ print "Done parsing!"
parseFiles (filename:xs) = do
  exists  <- liftIO $ doesFileExist filename
  case exists of
    False -> do
        liftIO $ print $ "Sorry, " ++ filename ++ " can't be found."
        parseFiles xs
    True  -> do
        content <- liftIO $ TL.readFile filename

        mapM_ (parseWordsToDB . T.words) $ T.lines content

        liftIO $ print $ "Parsed file " ++ filename ++ "!..."
        parseFiles xs


updatePrefixList :: [Prefix] -> Prefix -> [Prefix]
updatePrefixList []                pre  = [pre]
updatePrefixList (p1@(Prefix w c):xs) p2@(Prefix w' c')
    | w == w'   = (Prefix w (c+c')) : xs
    | otherwise = p1 : updatePrefixList xs p2


-- | Single Connection with DB
--   All learning access to DB is called within this function
learnAction :: Text -> [FilePath] -> IO()
learnAction fp fps = runSqlite fp $ do
    runMigration migrateAll

    parseFiles fps

    return ()

-- ####################             ####################
-- #################### GET Request ####################
-- ####################             ####################

-- | Every 'GET' request to the DB is called within this function
getAction :: Text -> Int -> IO()
getAction fp n = runSqlite fp $ do
    runMigration migrateAll

    let sql = "Select Count(*) FROM WORD;"
    [[PersistInt64 i]] <- rawQuery sql [] $$ consume
    let dbRowCount = fromIntegral i :: Integer

    sentence <- constructSentence dbRowCount n Nothing

    liftIO $ print $ T.drop 1 sentence

    return ()

-- Ques in the DB and constructs a sentence with N thunks
constructSentence :: Integer     -- DB size
                  -> Int         -- thunks
                  -> Maybe Word
                  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) T.Text

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
    return $ T.unwords [rest, suf]

-- word does have prefixes -> choose a semi-random one
constructSentence dbSize thunks (Just (Word suf ps)) = do
    let psmax = foldl' (\acc (Prefix _ val) -> val + acc) 0 ps
    r <- liftIO $ randomRIO (0, psmax)
    let prefix = getSemiRandomThunk ps r
        newsuffix = T.words prefix !! 0
    mword <- getBy $ UniqueSuffix newsuffix
    rest <- constructSentence dbSize (thunks - 1) (stripEntity mword)
    return $ T.unwords [rest, prefix]

getSemiRandomThunk :: [Prefix] -> Int -> T.Text
getSemiRandomThunk []     _ = "__error__"
getSemiRandomThunk ((Prefix w n):xs) c
    | c - n <= 0 = w
    | otherwise  = getSemiRandomThunk xs (c-n)

stripEntity :: Maybe (Entity Word) -> Maybe Word
stripEntity (Just (Entity _ word)) = Just word
stripEntity Nothing                = Nothing

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