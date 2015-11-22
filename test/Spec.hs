import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib
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

  describe "Eq" $ do
    it "compares doubles" $
      (1234567890 :: Double) `shouldBe` (1234567890 :: Double)

  describe "Lib" $ do
    it "exists"
       someFunc
    it "aesonTest"
       aesonTest

  describe "Lib.decodeCoord" $ do
    it "decodes Coord JSON" $
       decodeCoord (BL.pack "{\"x\":3.0,\"y\":-1.0}") `shouldBe` Just (Coord 3.0 (-1.0))
    it "decodes Actor JSON" $
       decodeActor (BL.pack "{\"id\":12,\"pos\":{\"x\":3.0,\"y\":-1.0}}")
                   `shouldBe` Just (Actor 12 (Coord 3.0 (-1.0)))
    it "decodes Move JSON" $
       decodeMove (BL.pack "{\"actor\":123,\"from\":{\"time\":1234567890,\"pos\":{\"x\":1,\"y\":2}},\"to\":{\"time\":1234767890,\"pos\":{\"x\":2,\"y\":2}}}")
                  `shouldBe` Just (Move 123 (TimePos 1234567890 (Coord 1.0 2.0))
                                            (TimePos 1234767890 (Coord 2.0 2.0)))
