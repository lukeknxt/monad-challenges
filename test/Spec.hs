import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Set1
import           MCPrelude
import           Crypto.Hash

import           Data.ByteString.Char8
import           Data.ByteString (ByteString)

main :: IO ()
main = hspec $ do 
  describe "fiveRands" $ 
    it "should multiply to 8681089573064486461641871805074254223660" $ 
      product fiveRands `shouldBe` 8681089573064486461641871805074254223660 