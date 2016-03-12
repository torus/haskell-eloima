{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Message
    ( decodeCoord
    , encodeCoord
    , decodeActor
    , encodeActor
    , decodeMove
    , encodeMove
    , Coord(Coord)
    , Actor(Actor)
    , TimePos(TimePos)
    , Move(Move)
    ) where

import GHC.Generics
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

data Coord = Coord { x :: Double, y :: Double }
             deriving (Show, Eq, Generic)

-- A ToJSON instance allows us to encode a value as JSON.

instance ToJSON Coord where
  toEncoding = genericToEncoding defaultOptions

data TimePos = TimePos { time :: Double, pos :: Coord }
               deriving (Show, Eq, Generic)

instance ToJSON TimePos where
  toEncoding = genericToEncoding defaultOptions

data Actor = Actor { id :: Int, lastMove :: Move }
             deriving (Show, Eq, Generic)

instance ToJSON Actor where
  toEncoding = genericToEncoding defaultOptions

data Move = Move { from :: TimePos, to :: TimePos }
            deriving (Show, Eq, Generic)

instance ToJSON Move where
  toEncoding = genericToEncoding defaultOptions

-- A FromJSON instance allows us to decode a value from JSON.  This
-- should match the format used by the ToJSON instance.

instance FromJSON Coord where
  parseJSON (Object v) = Coord <$>
                         v .: "x" <*>
                         v .: "y"
  parseJSON _          = empty

instance FromJSON Actor where
  parseJSON (Object a) = Actor <$>
                         a .: "id" <*>
                         a .: "lastMove"
  parseJSON _          = empty

instance FromJSON TimePos where
  parseJSON (Object a) = TimePos <$>
                         a .: "time" <*>
                         a .: "pos"
  parseJSON _          = empty

instance FromJSON Move where
  parseJSON (Object m) = Move <$>
                         m .: "from" <*>
                         m .: "to"
  parseJSON _          = empty

decodeCoord :: BL.ByteString -> Maybe Coord
decodeCoord = decode

encodeCoord :: Coord -> BL.ByteString
encodeCoord = encode

decodeActor :: BL.ByteString -> Maybe Actor
decodeActor = decode

encodeActor :: Actor -> BL.ByteString
encodeActor = encode

decodeMove :: BL.ByteString -> Maybe Move
decodeMove = decode

encodeMove :: Move -> BL.ByteString
encodeMove = encode
