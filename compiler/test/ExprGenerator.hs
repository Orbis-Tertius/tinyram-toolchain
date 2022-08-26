
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module ExprGenerator where
import           Data.Foldable                        (toList)
import           Data.Maybe
import           Data.Type.Equality
import qualified Test.QuickCheck                      as QC

import           Model

import qualified Data.ByteString                      as BS
import           Test.QuickCheck.Instances.ByteString

findVarsOf' :: Type a -> Context -> [(Name, Type a)]
findVarsOf' _ (Context []) = []
findVarsOf' type' (Context ((s, ATypeC t):xs)) = case type' `eqTy1` t of
    Just Refl -> (s, t) : rest
    Nothing   -> rest
  where
    rest = findVarsOf' type' (Context xs)

instance QC.Arbitrary ACType where
    arbitrary = QC.frequency
      [
        (2, return (ACTypeC TInteger))
      , (2, return (ACTypeC TBool))
      , (2, return (ACTypeC TUnit))
      , (2, return (ACTypeC TByteString))
      , (1, do
              (ACTypeC a) <- QC.arbitrary
              return (ACTypeC $ TList a))
      , (1, do
              (ACTypeC a) <- QC.arbitrary
              (ACTypeC b) <- QC.arbitrary
              return (ACTypeC $ TPair a b)
        )
      ]

instance QC.Arbitrary AType where
    arbitrary = QC.frequency
      [
        (1, do
              (ACTypeC a) <- QC.arbitrary
              return (ATypeC $ CType a)
        )
      , (1, do
              (ATypeC a) <- QC.arbitrary
              (ATypeC b) <- QC.arbitrary
              return (ATypeC $ TArrow a b)
        )
      ]

instance QC.Arbitrary AExpr where
    arbitrary = do
      (ACTypeC type') <- QC.arbitrary
      AExprC <$> genTypable emptyContext (CType type')

genApp :: Context -> Type a -> QC.Gen (Expr a)
genApp context type' = do
  (ATypeC type1) <- QC.arbitrary
  l <- genTypable context (TArrow type1 type')
  r <- genTypable context type1
  return $ App l r

genArrow :: Context -> (Type a, Type b) -> QC.Gen (Expr (a -> b))
genArrow (Context bindings) (argT, bodyT) = do
  let name = case bindings of
        (Name name, _) : _ -> Name (name + 1)
        []                 -> Name 0
  body <- genTypable (Context ((name, ATypeC argT) : bindings)) bodyT
  return $ Lambda name argT body

getVar :: Context -> Type a -> Maybe (QC.Gen (Expr a))
getVar context type' =
  case vars of
    _:_ -> Just $ uncurry Var <$> QC.elements vars
    []  -> Nothing
  where
    vars = findVarsOf' type' context

genByBuiltin' :: forall a b. Context -> (Expr a, a `EndsWith` b) -> QC.Gen (Expr b)
genByBuiltin' context (expr, proof) = do
  case proof of
    C1 Refl -> return expr
    C2 typeC ew -> do
      c <- genTypable context typeC
      genByBuiltin' context (App expr c, ew)

genDelay :: Context -> Type a -> QC.Gen (Expr (Wrap a))
genDelay context type' = do
  v <- genTypable context type'
  return $ Delay v

genForce :: Context -> Type a -> QC.Gen (Expr a)
genForce context type' = do
  v <- genTypable context (TWrap type')
  return $ Force v

data ResultsIn b where
  ResultsIn :: (Op a, a `EndsWith` b) -> ResultsIn b

genByBuiltin :: forall a b. Context -> ResultsIn b -> QC.Gen (Expr b)
genByBuiltin context (ResultsIn (nop, proof)) = genByBuiltin' context (Op nop, proof)

integerByBuiltin :: QC.Gen (ResultsIn Integer)
integerByBuiltin =
  QC.elements $
    (
      ResultsIn . (, TInteger ~> TInteger ~> refl) <$>
        [ AddInteger
        , SubtractInteger
        , MultiplyInteger
        , DivideInteger
        , QuotientInteger
        , RemainderInteger
        , ModInteger
        ]
    )
    ++
    (
      ResultsIn . (, TByteString ~> TInteger ~> refl) <$>
        [ IndexByteString
        ]
    )
    ++
    (
      ResultsIn . (, TByteString ~> refl) <$>
        [ LengthOfByteString
        ]
    )

boolByBuiltin :: QC.Gen (ResultsIn Bool)
boolByBuiltin = do
  (ACTypeC nestedType) <- QC.arbitrary
  QC.elements $
    (
      ResultsIn . (, TInteger ~> TInteger ~> refl) <$>
        [ EqualsInteger
        , LessThanInteger
        , LessThanEqualsInteger
        ]
    )
    ++
    (
      ResultsIn . (, TByteString ~> TByteString ~> refl) <$>
        [ EqualsByteString
        , LessThanByteString
        , LessThanEqualsByteString
        ]
    )
    ++
    (
      ResultsIn . (, TList nestedType ~> refl) <$>
        [
          NullList
        ]
    )

listByBuiltin :: CType a -> QC.Gen (ResultsIn [a])
listByBuiltin type' = do
   QC.elements $
    (
      ResultsIn . (, TList type' ~> refl) <$>
        [
          TailList
        ]
    )
    ++
    (
      ResultsIn . (, type' ~> TList type' ~> refl) <$>
        [
          MkCons
        ]
    )


byteStringByBuiltin :: QC.Gen (ResultsIn BS.ByteString)
byteStringByBuiltin =
  QC.elements $
    (
      ResultsIn . (, TByteString ~> TByteString ~> refl) <$>
        [ AppendByteString
        ]
    )
    ++
    (
      ResultsIn . (, TInteger ~> TByteString ~> refl) <$>
        [ ConsByteString
        ]
    )
    ++
    (
      ResultsIn . (, TInteger ~> TInteger ~> TByteString ~> refl) <$>
        [ SliceByteString
        ]
    )

anyByBuiltin :: Type a -> QC.Gen (ResultsIn a)
anyByBuiltin type' = do
  (ACTypeC nestedType) <- QC.arbitrary
  let cTypeSpecific = case type' of
        CType ct ->
          (
            ResultsIn . (, TPair ct nestedType ~> refl) <$>
              [ FstPair
              ]
          )
          ++
          (
            ResultsIn . (, TPair nestedType ct ~> refl) <$>
              [ SndPair
              ]
          )
          ++
          (
            ResultsIn . (, TList ct ~> refl) <$>
              [ HeadList
              ]
          )
        _ -> []
  QC.elements $
    (
      ResultsIn . (, TUnit ~> C2 type' refl) <$>
        [ ChooseUnit
        ]
    )
    ++
    (
      ResultsIn . (, TList nestedType ~> C2 type' (C2 type' refl)) <$>
        [ ChooseList
        ]
    )
    ++
    (
      ResultsIn . (, TBool ~> C2 type' (C2 type' refl)) <$>
        [ If
        ]
    )
    ++
    cTypeSpecific

genLit :: CType a -> QC.Gen a
genLit type' = case type' of
  TInteger     -> QC.arbitrary
  TBool        -> QC.arbitrary
  TUnit        -> QC.arbitrary
  TByteString  -> QC.arbitrary
  TPair ct ct' -> do
    a <- genLit ct
    b <- genLit ct'
    return (a, b)
  TList ct     -> do
    unitList <- QC.resize 5 (QC.arbitrary :: QC.Gen [()])
    sequence (genLit ct <$ unitList)

genTypable :: forall a. Context -> Type a -> QC.Gen (Expr a)
genTypable context type' =
  QC.frequency $ specific ++ common
  where
    specific :: [(Int, QC.Gen (Expr a))]
    specific = case type' of
      TArrow l r ->
        [ (10, genArrow context (l, r))
        ]
      TWrap nestedType ->
        [ (10, genDelay context nestedType)
        ]
      CType cType -> (10, Lit cType <$> genLit cType) : case cType of
        TInteger ->
          [ (5, genByBuiltin context =<< integerByBuiltin)
          ]
        TBool ->
          [ (5, genByBuiltin context =<< boolByBuiltin)
          ]
        TByteString ->
          [ (5, genByBuiltin context =<< byteStringByBuiltin)
          ]
        TList nestedType ->
          [ (5, genByBuiltin context =<< listByBuiltin nestedType)
          ]
        TPair typeA typeB -> []
        TUnit -> []

    common =
      [ (1, genApp context type')
      , (1, genByBuiltin context =<< anyByBuiltin type')
      , (1, genForce context type')
      -- , (1, return $ Error type')
      ]
      ++
      toList ((10 :: Int, ) <$> getVar context type')

