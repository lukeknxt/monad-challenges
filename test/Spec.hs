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
  describe "randString3" $ 
    it "should sha256 to 9d475eb78d3e38085220ed6ebde9d8f7d26540bb1c8f9382479c3acd4c8c94a3" $ 
      show (hashWith SHA256 $ pack randString3) `shouldBe` "9d475eb78d3e38085220ed6ebde9d8f7d26540bb1c8f9382479c3acd4c8c94a3"