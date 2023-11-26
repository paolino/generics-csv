# Haskell library with (SOP) record support for Cassava

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Data.ByteString (ByteString)
import Data.CSV.SOP
    ( Column (..)
    , GenericsCSV (..)
    , P (..)
    , R (..)
    , column
    )
import Data.Csv
    ( FromField (..)
    , FromNamedRecord (..)
    , ToField (..)
    , ToNamedRecord (..)
    , runParser
    )
import Generics.SOP (NP (Nil, (:*)))
import Generics.SOP.TH (deriveGeneric)

import qualified Data.ByteString.Char8 as B
import qualified Data.Time as Time
import qualified Data.Time.Calendar as Time

newtype Title = Title ByteString
    deriving newtype (Show, Eq, FromField, ToField)
newtype Author = Author ByteString
    deriving newtype (Show, Eq, FromField, ToField)
newtype ISBN = ISBN ByteString
    deriving newtype (Show, Eq, FromField, ToField)
newtype Day = Day Time.Day
    deriving newtype (Show, Eq)

dayColumn :: ByteString -> Column Day
dayColumn x =
    Column
        { columnName = x
        , renderColumn = R $ \(Day d) ->
            B.pack
                $ Time.formatTime Time.defaultTimeLocale format d
        , parseColumn =
            P
                $ fmap Day
                    . Time.parseTimeM True Time.defaultTimeLocale format
                    . B.unpack
        }
  where
    format = "%d/%m/%Y"

data Book where
    Book
        :: { title :: Title
           , author :: Author
           , isbn13 :: ISBN
           , dateAdded :: Day
           }
        -> Book
    deriving stock (Show, Eq)

deriveGeneric ''Book

instance GenericsCSV Book where
    codec _ =
        column "Title"
            :* column "Author"
            :* column "ISBN"
            :* dayColumn "Date Added"
            :* Nil

b0 :: Book
b0 =
    Book
        (Title "foo")
        (Author "bar")
        (ISBN "baz")
        (Day $ Time.fromGregorian 2020 1 1)

roundtrip :: (Eq a, FromNamedRecord a, ToNamedRecord a) => a -> Bool
roundtrip x = runParser (parseNamedRecord (toNamedRecord x)) == Right x

test :: Bool
test = roundtrip b0

main :: IO ()
main = print test

```