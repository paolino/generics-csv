
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.CSV.SOP
    ( Column (..)
    , GenericsCSV (..)
    , P (..)
    , R (..)
    , column
    )
where

import Data.ByteString (ByteString)
import Data.Csv
    ( Field
    , FromField (..)
    , FromNamedRecord (..)
    , NamedRecord
    , Parser
    , ToField (..)
    , ToNamedRecord (..)
    , namedRecord
    , (.:)
    )
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Invariant (Invariant (..))
import Data.Time (parseTimeM)
import Generics.SOP
    ( I (..)
    , IsProductType
    , K (..)
    , NP (..)
    , ProductCode
    , Proxy (..)
    , productTypeFrom
    , productTypeTo
    )
import Generics.SOP.NP
    ( collapse_NP
    , map_NP
    , sequence_NP
    , trans_NP
    , zipWith_NP
    )

import qualified Data.ByteString.Char8 as B
import qualified Data.Time as Time

-- | A render to 'Field'
newtype R a = R {unR :: a -> Field}

instance Contravariant R where
    contramap f (R r) = R (r . f)

-- | Parser from 'Field'
newtype P a = P {unP :: Field -> Parser a}
    deriving (Functor)

-- | A column level parse and render with column name
data Column a = Column
    { columnName :: ByteString
    -- ^ column name
    , renderColumn :: R a
    -- ^ how to render 'a'
    , parseColumn :: P a
    -- ^ how to parse 'a'
    }

instance Invariant Column where
    invmap f g (Column c r p) =
        Column c (contramap g r) (fmap f p)

getParser :: NamedRecord -> Column a -> Parser a
getParser r (Column c _ (P p)) = r .: c >>= p

getRender :: I a -> Column a -> K (ByteString, Field) a
getRender (I x) (Column c (R f) _) = K (c, f x)

-- | Define a 'Column' for a type that has a 'ToField' and 'FromField' instance
column :: (ToField a, FromField a) => ByteString -> Column a
column c = Column c (R toField) (P parseField)

-- | Define 'Column's for a product type
class IsProductType a (ProductCode a) => GenericsCSV a where
    codec :: Proxy a -> NP Column (ProductCode a)

instance GenericsCSV a => FromNamedRecord a where
    parseNamedRecord :: GenericsCSV a => NamedRecord -> Parser a
    parseNamedRecord r =
        let
            np = codec (Proxy @a)
        in
            productTypeTo <$> sequence_NP (map_NP (getParser r) np)

instance GenericsCSV a => ToNamedRecord a where
    toNamedRecord :: GenericsCSV a => a -> NamedRecord
    toNamedRecord x =
        let
            np = codec (Proxy @a)
            fields = productTypeFrom x
        in
            namedRecord $ collapse_NP (zipWith_NP getRender fields np)