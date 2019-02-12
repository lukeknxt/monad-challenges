module Set2Spec where

import           Test.Hspec
import           MCPrelude
import Set2

spec :: Spec
spec = do
  describe "headMay" $ do
    it "should handle empty list" $ 
      shouldBe (headMay [] :: Maybe Integer) $ Nothing
    it "should handle single element list" $ 
      shouldBe (headMay [1]) $ Just 1
    it "should handle multi-element list" $ 
      shouldBe (headMay [1, 2]) (Just 1)
  describe "tailMay" $ do
    it "should handle empty list" $ 
      shouldBe (tailMay [] :: Maybe [Integer]) $ Nothing
    it "should handle single element list" $ 
      shouldBe (tailMay [1]) $ Just [] 
    it "should handle multi-element list" $ 
      shouldBe (tailMay [1, 2]) (Just [2])
    it "should handle multi-element tail" $ 
      shouldBe (tailMay [1, 2, 3]) (Just [2, 3])
  describe "lookupMay" $ do
    it "should handle empty list" $ 
      shouldBe (lookupMay 1 [] :: Maybe Integer) $ Nothing
    it "should handle single element list" $ 
      shouldBe (lookupMay 1 [(1, 2)]) $ Just 2
    it "should handle multi-element list" $ 
      shouldBe (lookupMay 3 [(1, 2), (3, 4)]) $ Just 4
  describe "divMay" $ do
    it "should handle division by zero" $ 
      shouldBe (divMay 2 0) $ Nothing
    it "should handle division by non-zero" $ 
      shouldBe (divMay 2 1) $ Just 2
  describe "maximumMay" $ do
    it "should handle empty list" $ 
      shouldBe (maximumMay [] :: Maybe Integer) $ Nothing
    it "should handle single element list" $ 
      shouldBe (maximumMay [1]) $ Just 1
    it "should handle multi-element list" $ 
      shouldBe (maximumMay [1, 2]) $ Just 2
  describe "minimumMay" $ do
    it "should handle empty list" $ 
      shouldBe (minimumMay [] :: Maybe Integer) $ Nothing
    it "should handle single element list" $ 
      shouldBe (minimumMay [1]) $ Just 1
    it "should handle multi-element list" $ 
      shouldBe (minimumMay [1, 2]) $ Just 1
