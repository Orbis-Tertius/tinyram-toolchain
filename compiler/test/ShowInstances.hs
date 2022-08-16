{-# LANGUAGE GADTs #-}
module ShowInstances (Show2DeBruijnTerm(..), Show2Data(..)) where

import           Data.Bifunctor     (Bifunctor (bimap))
import qualified Data.ByteString    as BS
import qualified Data.List          as L
import           GHC.Exts           (toList)
import qualified PlutusCore.Data    as D
import           PlutusCore.Default
import qualified PlutusCore.Error   as UPLC (Error)
import qualified UntypedPlutusCore  as UPLC

type DeBruijnTerm = UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun
type DefaultError = UPLC.Error UPLC.DefaultUni UPLC.DefaultFun

newtype Show2DeBruijnTerm = Show2DeBruijnTerm (DeBruijnTerm ())

newtype Show2Data = Show2Data D.Data

instance Show Show2DeBruijnTerm where
  show (Show2DeBruijnTerm term) = case term of
    UPLC.Var () (UPLC.DeBruijn (UPLC.Index i)) -> "(Idx " ++ show i ++ ")"
    UPLC.LamAbs () _ te                        -> "(Lam " ++ show te ++ ")"
    UPLC.Apply () te te'                       -> "(App " ++ show te ++ " " ++ show te' ++ ")"
    UPLC.Force () te                           -> "(For " ++ show te ++ ")"
    UPLC.Delay () te                           -> "(Del " ++ show te ++ ")"
    UPLC.Constant () so                        -> showConstant so
    UPLC.Builtin () df                         -> show df
    UPLC.Error ()                              -> "error"

instance Show Show2Data where
  show (Show2Data data') = case data' of
    D.Constr n das -> "(Constr " ++ show n ++ " " ++ show (Show2Data <$> das) ++ ")"
    D.Map x0       -> "(Map " ++ show (bimap Show2Data Show2Data <$> x0) ++ ")"
    D.List das     -> "(List " ++ show (Show2Data <$> das) ++ ")"
    D.I n          -> "(Integer " ++ show n ++ ")"
    D.B bs         -> "(BS " ++ showBytestring bs ++ ")"

showBytestring :: BS.ByteString -> String
showBytestring bs = let l1 = fromIntegral <$> toList bs :: [Integer]
                        l2 = L.concat . L.intersperse "," $ show <$> l1
                     in "{" ++ l2 ++ "}"

showConstant :: Some (ValueOf UPLC.DefaultUni) -> String
showConstant constant = case constant of
    Some (ValueOf DefaultUniInteger v)         -> show v
    Some (ValueOf DefaultUniBool v)            -> show v
    Some (ValueOf DefaultUniUnit v)            -> show v
    Some (ValueOf DefaultUniByteString bs)     -> showBytestring bs
    Some (ValueOf DefaultUniString s)          -> show s
    Some (ValueOf (DefaultUniPair xUni yUni) (x, y)) ->
      "(" ++ showConstant (Some (ValueOf xUni x)) ++
      "," ++ showConstant (Some (ValueOf yUni y)) ++
      ")"
    Some (ValueOf (DefaultUniList eUni) list)     ->
      let l = showConstant . Some . ValueOf eUni <$> list in
          "[" ++ L.intercalate "," l ++ "]"
    Some (ValueOf DefaultUniData d) -> show $ Show2Data d
    _ -> undefined
