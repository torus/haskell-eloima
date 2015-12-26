{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , aesonTest
    , decodeCoord
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

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

data Coord = Coord { x :: Double, y :: Double }
             deriving (Show, Eq)

-- A ToJSON instance allows us to encode a value as JSON.

instance ToJSON Coord where
  toJSON (Coord xV yV) = object [ "x" .= xV,
                                  "y" .= yV ]

data TimePos = TimePos { time :: Double, position :: Coord }
               deriving (Show, Eq)

instance ToJSON TimePos where
  toJSON (TimePos time pos) = object [ "time" .= time,
                                       "pos"  .= pos ]

data Actor = Actor { id :: Int, lastMove :: Move }
             deriving (Show, Eq)

instance ToJSON Actor where
  toJSON (Actor id move) = object [ "id"       .= id,
                                    "lastMove" .= move ]

data Move = Move { from :: TimePos, to :: TimePos }
            deriving (Show, Eq)

instance ToJSON Move where
  toJSON (Move from to) = object [ "from" .= from,
                                   "to"   .= to ]

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

aesonTest :: IO ()
aesonTest = do
  let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
  print req
  let reply = Coord 123.4 20
  BL.putStrLn (encode reply)
