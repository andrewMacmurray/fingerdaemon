{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple.Types
import Database.SQLite.Simple
import System.Environment (getArgs)
import Data.Text (Text, pack)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)

import User

-- database command line utility
-- create the db with  `stack exec dbuser -- init`
-- add a user with     `stack exec dbuser -- add name shell homeDir realName phone`
-- update a user with  `stack exec dbuser -- update name shell homeDir realName phone`
-- show all users with `stack exec dbuser -- all`

type DBR a = ReaderT Connection IO a

main :: IO ()
main = do
  xs   <- getArgs
  conn <- open "finger.db"
  let command = handleCommand xs
      read' c = runReaderT c conn
  maybe argsError read' $ command


argsError :: IO ()
argsError =
  putStrLn $
    concat [ "please enter correct command and args: "
           , "either `add` or `update` plus: "
           , "username, "
           , "shell, "
           , "homeDir, "
           , "real-name, "
           , "phone"
           ]

handleCommand :: [String] -> Maybe (DBR ())
handleCommand []     = Nothing
handleCommand [x]    = do
  case x of
    "init"   -> Just createDatabase
    "all"    -> Just showAllUsers
    _        -> Nothing

handleCommand (x:xs) = do
  user <- argsToRow . map pack $ xs
  case x of
    "add"    -> Just $ addUser user
    "update" -> Just $ updateUser user
    _        -> Nothing

argsToRow :: [Text] -> Maybe UserRow
argsToRow [un, sh, hd, rn, ph] = Just (Null, un, sh, hd, rn, ph)
argsToRow _                    = Nothing

createDatabase :: DBR ()
createDatabase = do
  conn <- ask
  io $ do
    execute_ conn createDbQuery
    putStrLn "db created"
    close conn

showAllUsers :: DBR ()
showAllUsers = do
  conn  <- ask
  users <- io (query_ conn allUsersQuery :: IO [User])
  io $ do
    putStrLn "all users"
    _ <- traverse (putStrLn . show) users
    close conn


deleteUserByName :: Text -> DBR ()
deleteUserByName username' = do
  runDb deleteUserQuery [username']
  io . putStrLn $ concat [ "user: ", show username', " deleted" ]

updateUser :: UserRow -> DBR ()
updateUser (Null, un, sh, hd, rn, p) = do
  let user = [ ":un" := un
             , ":sh" := sh
             , ":hd" := hd
             , ":rn" := rn
             , ":p"  := p
             ]
  runDbNamed updateUserQuery user
  io $ putStrLn "user updated"

addUser :: UserRow -> DBR ()
addUser user = do
  runDb insertUserQuery user
  io . putStrLn $ "User added " ++ show user

io :: IO a -> DBR a
io = liftIO

runDb :: ToRow a => Query -> a -> DBR ()
runDb query' xs = do
  conn <- ask
  io $ do
    execute conn query' xs
    close conn

runDbNamed :: Query -> [NamedParam] -> DBR ()
runDbNamed query' xs = do
  conn <- ask
  io $ do
    executeNamed conn query' xs
    close conn
