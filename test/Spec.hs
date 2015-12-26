import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Message
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $
      evaluate (head []) `shouldThrow` anyException

  describe "Eq" $
    it "compares doubles" $
      (1234567890 :: Double) `shouldBe` (1234567890 :: Double)

  describe "Message" $ do
    it "exists"
       someFunc

  describe "Message.decodeCoord" $
    it "decodes Coord JSON" $
       decodeCoord (BL.pack "{\"x\":3.0,\"y\":-1.0}") `shouldBe` Just (Coord 3.0 (-1.0))

  describe "Message.decodeActor" $
    it "decodes Actor JSON" $
       decodeActor (BL.pack "{\"id\":123,\"lastMove\":{\"from\":{\"time\":1234567890,\"pos\":{\"x\":1,\"y\":2}},\"to\":{\"time\":1234567891,\"pos\":{\"x\":2,\"y\":3}}}}")
                   `shouldBe` Just (Actor 123 (Move (TimePos 1234567890 (Coord 1 2))
                                                    (TimePos 1234567891 (Coord 2 3))))

  describe "Message.decodeMove" $
    it "decodes Move JSON" $
       decodeMove (BL.pack "{\"to\":{\"time\":1234767891,\"pos\":{\"x\":2,\"y\":2}},\"from\":{\"time\":1234567890,\"pos\":{\"x\":1,\"y\":2}}}")
                  `shouldBe` Just (Move (TimePos 1234567890 (Coord 1.0 2.0))
                                        (TimePos 1234767891 (Coord 2.0 2.0)))

  describe "Message.encodeCoord" $
    it "encodes coordinate" $
       encodeCoord (Coord 3.1 (-1.1)) `shouldBe` BL.pack "{\"x\":3.1,\"y\":-1.1}"

  describe "Message.encodeActor" $
    it "encodes actor" $
       encodeActor (Actor 12 (Move (TimePos 1234567890 (Coord 1.0 2.0))
                                   (TimePos 1234767891 (Coord 2.0 2.0))))
                   `shouldBe` BL.pack "{\"lastMove\":{\"to\":{\"time\":1234767891,\"pos\":{\"x\":2,\"y\":2}},\"from\":{\"time\":1234567890,\"pos\":{\"x\":1,\"y\":2}}},\"id\":12}"

  describe "Message.encodeMove" $
    it "encodes move" $
       encodeMove (Move (TimePos 1234567890 (Coord 1.0 2.0))
                        (TimePos 1234767891 (Coord 2.0 2.0)))
                  `shouldBe` BL.pack "{\"to\":{\"time\":1234767891,\"pos\":{\"x\":2,\"y\":2}},\"from\":{\"time\":1234567890,\"pos\":{\"x\":1,\"y\":2}}}"
