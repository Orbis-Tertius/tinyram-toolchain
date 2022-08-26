module DataGenerator () where

import           PlutusCore.Data
import qualified Test.QuickCheck                      as QC
import           Test.QuickCheck.Instances.ByteString

instance QC.Arbitrary Data where
  arbitrary = arbitrary' 0
    where
      arbitrary' :: Integer -> QC.Gen Data
      arbitrary' n = if n > 8
        then QC.oneof [bGen, iGen]
        else QC.oneof [bGen, iGen, mapGen (n + 1), listGen (n + 1), constrGen (n + 1)]

      bGen = B <$> QC.resize 16 QC.arbitrary
      iGen = I <$> QC.arbitrary

      listOf' n = QC.listOf $ arbitrary' n
      mapGen n = Map <$> QC.resize 5 (zip <$> listOf' n <*> listOf' n)
      listGen n = List <$> QC.resize 5 (listOf' n)
      constrGen n = uncurry Constr <$> ((,) <$> QC.arbitrary <*> QC.resize 5 (listOf' n))
