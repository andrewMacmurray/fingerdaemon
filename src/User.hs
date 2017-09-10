{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module User where

import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Data.Text (Text)
import Text.RawString.QQ

data User =
  User {
    userId :: Integer
  , username :: Text
  , shell :: Text
  , homeDirectory :: Text
  , realName :: Text
  , phone :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ un sh hd rn ph) =
    toRow (id_, un, sh, hd, rn, ph)

type UserRow =
  (Null, Text, Text, Text, Text, Text)


createDbQuery :: Query
createDbQuery = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT,
   homeDirectory TEXT,
   realName TEXT,
   phone TEXT)
|]

allUsersQuery :: Query
allUsersQuery =
  "SELECT * FROM users"

getUserQuery :: Query
getUserQuery =
  "SELECT * FROM users WHERE username = ?"

updateUserQuery :: Query
updateUserQuery = [r|
UPDATE users SET
  shell = :sh,
  homeDirectory = :hd,
  realName = :rn,
  phone = :p
WHERE username = :un
|]

insertUserQuery :: Query
insertUserQuery =
  "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

deleteUserQuery :: Query
deleteUserQuery =
  "DELETE FROM users WHERE username = ?"
