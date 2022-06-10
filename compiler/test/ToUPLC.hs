{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module ToUPLC where

import           Model
import qualified PlutusCore.Default          as UPLC
import           Test.QuickCheck             (Arbitrary (..))
import qualified UntypedPlutusCore           as UPLC
import qualified UntypedPlutusCore.Core.Type as UPLC

type NamedUPLCTerm = UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ()

ff :: CType a -> UPLC.DefaultUni (UPLC.Esc a)
ff a = case a of
  TInteger     -> UPLC.DefaultUniInteger
  TBool        -> UPLC.DefaultUniBool
  TUnit        -> UPLC.DefaultUniUnit
  TByteString  -> UPLC.DefaultUniByteString
  TPair ct ct' -> UPLC.DefaultUniPair (ff ct) (ff ct')
  TList ct     -> UPLC.DefaultUniList (ff ct)

toUPLC :: AExpr -> NamedUPLCTerm
toUPLC (AExprC e) = case e of
  Var name ty               -> UPLC.Var () (fromName name)
  Lambda name ty ex         -> UPLC.LamAbs () (fromName name) (toUPLC $ AExprC ex)
  App ex ex'                -> UPLC.Apply () (toUPLC $ AExprC ex) (toUPLC $ AExprC ex')
  Error ty                  -> UPLC.Error ()
  Delay ex                  -> UPLC.Delay () (toUPLC $ AExprC ex)
  Force ex                  -> UPLC.Force () (toUPLC $ AExprC ex)
  Lit type' v               -> case type' of
    TInteger    -> UPLC.Constant () (UPLC.Some (UPLC.ValueOf UPLC.DefaultUniInteger v))
    TBool       -> UPLC.Constant () (UPLC.Some (UPLC.ValueOf UPLC.DefaultUniBool v))
    TUnit       -> UPLC.Constant () (UPLC.Some (UPLC.ValueOf UPLC.DefaultUniUnit v))
    TByteString -> UPLC.Constant () (UPLC.Some (UPLC.ValueOf UPLC.DefaultUniByteString v))
    TPair a b   -> UPLC.Constant () (UPLC.Some (UPLC.ValueOf (UPLC.DefaultUniPair (ff a) (ff b)) v))
    TList l     -> UPLC.Constant () (UPLC.Some (UPLC.ValueOf (UPLC.DefaultUniList (ff l)) v))
  Op op                      -> case op of
    AddInteger               -> UPLC.Builtin () UPLC.AddInteger
    SubtractInteger          -> UPLC.Builtin () UPLC.SubtractInteger
    MultiplyInteger          -> UPLC.Builtin () UPLC.MultiplyInteger
    DivideInteger            -> UPLC.Builtin () UPLC.DivideInteger
    QuotientInteger          -> UPLC.Builtin () UPLC.QuotientInteger
    RemainderInteger         -> UPLC.Builtin () UPLC.RemainderInteger
    ModInteger               -> UPLC.Builtin () UPLC.ModInteger
    EqualsInteger            -> UPLC.Builtin () UPLC.EqualsInteger
    LessThanInteger          -> UPLC.Builtin () UPLC.LessThanInteger
    LessThanEqualsInteger    -> UPLC.Builtin () UPLC.LessThanEqualsInteger
    AppendByteString         -> UPLC.Builtin () UPLC.AppendByteString
    ConsByteString           -> UPLC.Builtin () UPLC.ConsByteString
    IndexByteString          -> UPLC.Builtin () UPLC.IndexByteString
    EqualsByteString         -> UPLC.Builtin () UPLC.EqualsByteString
    LessThanByteString       -> UPLC.Builtin () UPLC.LessThanByteString
    LessThanEqualsByteString -> UPLC.Builtin () UPLC.LessThanEqualsByteString
    MkCons                   -> force $ UPLC.Builtin () UPLC.MkCons
    ChooseUnit               -> force $ UPLC.Builtin () UPLC.ChooseUnit
    LengthOfByteString       -> UPLC.Builtin () UPLC.LengthOfByteString
    FstPair                  -> force $ force $ UPLC.Builtin () UPLC.FstPair
    SndPair                  -> force $ force $ UPLC.Builtin () UPLC.SndPair
    NullList                 -> force $ UPLC.Builtin () UPLC.NullList
    TailList                 -> force $ UPLC.Builtin () UPLC.TailList
    HeadList                 -> force $ UPLC.Builtin () UPLC.HeadList
    SliceByteString          -> UPLC.Builtin () UPLC.SliceByteString
    ChooseList               -> force $ force $ UPLC.Builtin () UPLC.ChooseList
    If                       -> force $ UPLC.Builtin () UPLC.IfThenElse
  where
      fromName :: Name -> UPLC.Name
      fromName (Name name) = UPLC.Name "" (UPLC.Unique $ fromIntegral name)

      force = UPLC.Force ()
