{-# language NoMonomorphismRestriction #-}

import Lesson_7_2.Monoidal

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

-- Maybe isomorphic type in terms of Sum
type MaybeC a = Sum Const Identity a
----------------------------------------



-- list isomorphic type
newtype ListC a = ListC (Sum Const (Product Identity ListC) a) deriving (Show,Eq)

instance Functor ListC where
  fmap f (ListC x) = ListC $ fmap f x

emptyC = ListC (LeftC Const)
(#) x = ListC . RightC . Product (Identity x)
infixr #
---------------------------------

main :: IO ()
main = hspec $ do
  describe "MaybeC is a Hask functor" $ do

    it "fmapping (+1) to isomorphic to Nothing" $ do
      (fmap (+1) (LeftC Const) :: MaybeC Int) `shouldBe` LeftC Const

    it "fmapping (+1) to isomorphic to (Just 1)" $ do
      (fmap (+1) (RightC $ Identity 1) :: MaybeC Int) `shouldBe` RightC (Identity 2)

  describe "ListC is a Hask functor" $ do
    it "fmapping (+1) to isomorphic to []" $ do
      (fmap (+1) (ListC (LeftC Const))) `shouldBe` (ListC (LeftC Const))

    it "fmapping (+1) to isomorphic to [1,2]" $ do

      fmap (+1) (1 # 2 # emptyC) `shouldBe` (2 # 3 # emptyC)






