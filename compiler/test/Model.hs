{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Model where

import qualified Data.ByteString    as BS
import           Data.Type.Equality

data Wrap a

data Constant a

data CType :: * -> * where
  TInteger    :: CType Integer
  TBool       :: CType Bool
  TUnit       :: CType ()
  TByteString :: CType BS.ByteString
  TPair       :: CType a -> CType b -> CType (a, b)
  TList       :: CType a -> CType [a]

data Type :: * -> * where
  CType       :: CType a -> Type a
  TArrow      :: Type a -> Type b -> Type (a -> b)
  TWrap       :: Type a -> Type (Wrap a)

data Op :: * -> * where
  AddInteger                :: Op (Integer -> Integer -> Integer)
  SubtractInteger           :: Op (Integer -> Integer -> Integer)
  MultiplyInteger           :: Op (Integer -> Integer -> Integer)
  DivideInteger             :: Op (Integer -> Integer -> Integer)
  QuotientInteger           :: Op (Integer -> Integer -> Integer)
  RemainderInteger          :: Op (Integer -> Integer -> Integer)
  ModInteger                :: Op (Integer -> Integer -> Integer)
  EqualsInteger             :: Op (Integer -> Integer -> Bool)
  LessThanInteger           :: Op (Integer -> Integer -> Bool)
  LessThanEqualsInteger     :: Op (Integer -> Integer -> Bool)
  AppendByteString          :: Op (BS.ByteString -> BS.ByteString -> BS.ByteString)
  ConsByteString            :: Op (Integer -> BS.ByteString -> BS.ByteString)
  IndexByteString           :: Op (BS.ByteString -> Integer -> Integer)
  EqualsByteString          :: Op (BS.ByteString -> BS.ByteString -> Bool)
  LessThanByteString        :: Op (BS.ByteString -> BS.ByteString -> Bool)
  LessThanEqualsByteString  :: Op (BS.ByteString -> BS.ByteString -> Bool)
  MkCons                    :: Op (a -> [a] -> [a])
  ChooseUnit                :: Op (() -> a -> a)

  LengthOfByteString        :: Op (BS.ByteString -> Integer)
  FstPair                   :: Op ((a, b) -> a)
  SndPair                   :: Op ((a, b) -> b)
  NullList                  :: Op ([a] -> Bool)
  TailList                  :: Op ([a] -> [a])
  HeadList                  :: Op ([a] -> a)

  SliceByteString           :: Op (Integer -> Integer -> BS.ByteString -> BS.ByteString)
  ChooseList                :: Op ([a] -> b -> b -> b)
  If                        :: Op (Bool -> a -> a -> a)

data a `EndsWith` b where
  C1 :: (a :~: b) -> (a `EndsWith` b)
  C2 :: Type c -> (a `EndsWith` b) -> ((c -> a) `EndsWith` b)

refl = C1 Refl

(~>) :: CType c -> (a `EndsWith` b) -> ((c -> a) `EndsWith` b)
(~>) t = C2 (CType t)

infixr 6 ~>

newtype Name = Name Integer

data Expr :: * -> * where
  Lit     :: CType a -> a -> Expr a
  Var     :: Name -> Type a -> Expr a
  Lambda  :: Name -> Type a -> Expr b -> Expr (a -> b)
  App     :: Expr (a -> b) -> Expr a -> Expr b
  Op      :: Op a -> Expr a
  Error   :: Type a -> Expr a
  Delay   :: Expr a -> Expr (Wrap a)
  Force   :: Expr (Wrap a) -> Expr a

eqTy :: CType u -> CType v -> Maybe (u :~: v)
eqTy TInteger  TInteger  = Just Refl
eqTy TBool TBool = Just Refl
eqTy (TPair u1 u2) (TPair v1 v2) = do
  Refl <- eqTy u1 v1
  Refl <- eqTy u2 v2
  return Refl
eqTy TUnit TUnit = Just Refl
eqTy TByteString TByteString = Just Refl
eqTy (TList l1) (TList l2) = do
  Refl <- eqTy l1 l2
  return Refl
eqTy _     _     = Nothing

eqTy1 :: Type u -> Type v -> Maybe (u :~: v)
eqTy1 (TArrow u1 u2) (TArrow v1 v2) = do
  Refl <- eqTy1 u1 v1
  Refl <- eqTy1 u2 v2
  return Refl
eqTy1 (TWrap a1) (TWrap a2) = do
  Refl <- eqTy1 a1 a2
  return Refl
eqTy1 (CType u1) (CType u2) = case eqTy u1 u2 of
  Nothing   -> Nothing
  Just Refl -> Just Refl
eqTy1 _ _ = Nothing

data ACType where
    ACTypeC :: CType a -> ACType

data AType where
    ATypeC :: Type a -> AType

data AExpr where
    AExprC :: Expr a -> AExpr

newtype Context = Context
  {
    bindings :: [(Name, AType)]
  }

emptyContext :: Context
emptyContext = Context []
