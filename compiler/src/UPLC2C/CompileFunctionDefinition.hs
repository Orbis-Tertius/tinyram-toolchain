{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications #-}

module UPLC2C.CompileFunctionDefinition ( compileFunctionDefinition ) where


import qualified Data.ByteString.Char8            as BS
import           Data.List                        (intersperse)
import           Data.Text                        (pack)
import           GHC.Exts                         (toList)
import           Text.Printf                      (printf)
import           Text.RawString.QQ                (r)

import           Data.Foldable                    (fold)
import           Data.Text.Encoding               (encodeUtf8)
import           UPLC2C.Prelude
import           UPLC2C.Types.CCode               (CCode (..))
import           UPLC2C.Types.CFunctionDefinition (CFunctionDefinition (..))
import           UPLC2C.Types.CName               (CName (..))
import           UPLC2C.Types.DeBruijnIndex       (DeBruijnIndex (..))

import qualified Data.Word                        as Word
import qualified Data.Int                         as Int

compileFunctionDefinition :: CName -> CFunctionDefinition -> CCode
compileFunctionDefinition name =
  \case
    (VariableReference i)              -> compileVariableReference name i
    (CreateClosureOver ref)            -> compileCreateClosure name ref
    (Apply operator operand)           -> compileApply name operator operand
    (Delay operand)                    -> compileDelay name operand
    (Force operand)                    -> compileForce name operand
    (ConstantInteger val)              -> compileConstantInteger name val
    (ConstantBool val)                 -> compileConstantBool name val
    ConstantUnit                       -> compileConstantUnit name
    ConstantByteString val             -> compileConstantByteString name val
    ConstantString val                 -> compileConstantString name val
    ConstantPair x y                   -> compileConstantPair name x y
    ConstantList list                  -> compileConstantList name list
    ConstantDataConstr integer list    -> compileDataConstr name integer list
    ConstantDataMap pairList           -> compileDataMap name pairList
    ConstantDataList list              -> compileDataList name list
    ConstantDataInteger integer        -> compileDataInteger name integer
    ConstantDataByteString  byteString -> compileDataByteString name byteString


compileVariableReference :: CName -> DeBruijnIndex -> CCode
compileVariableReference (CName fnName) (DeBruijnIndex i) =
  CCode . pack $ printf variableReferenceTemplate fnName i


variableReferenceTemplate :: String
variableReferenceTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  for (WORD i = 1; i < %i; i++) {
    scope = scope->rest;
  }
  return scope->first;
}
  |]


compileCreateClosure :: CName -> CName -> CCode
compileCreateClosure (CName name) (CName ref) =
  CCode . pack $ printf createClosureTemplate name ref


createClosureTemplate :: String
createClosureTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = FunctionType;
  result->value.fn.apply = &%s;
  result->value.fn.scope = scope;
  return result;
}
  |]


compileApply :: CName -> CName -> CName -> CCode
compileApply (CName name) (CName operator) (CName operand) =
  CCode . pack $ printf applyTemplate name operator operand


applyTemplate :: String
applyTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  const struct NFData *operatorResult = %s(scope);
  const struct NFData *operandResult = %s(scope);
  if (operatorResult->type == FunctionType) {
    struct LexicalScope *new_scope = (struct LexicalScope *)alloc(sizeof(struct LexicalScope));
    new_scope->first = operandResult;
    new_scope->rest = operatorResult->value.fn.scope;

    return operatorResult->value.fn.apply(new_scope);
  } else {
    diverge();
  }
}
  |]


compileDelay :: CName -> CName -> CCode
compileDelay (CName name) (CName operand) =
  CCode . pack $ printf delayTemplate name operand


delayTemplate :: String
delayTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = ThunkType;
  result->value.thunk.run = %s;
  result->value.thunk.scope = scope;
  return result;
}
  |]


compileForce :: CName -> CName -> CCode
compileForce (CName name) (CName operand) =
  CCode . pack $ printf forceTemplate name operand


forceTemplate :: String
forceTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  const struct NFData *operandResult = %s(scope);
  if (operandResult->type == ThunkType) {
    struct Thunk thunk = operandResult->value.thunk;
    return thunk.run(thunk.scope);
  } else {
    diverge();
  }
}
  |]

compileConstantInteger :: CName -> Integer -> CCode
compileConstantInteger (CName name) val
  | val >= 0 && val <= fromIntegral (maxBound @Word.Word32) =
    CCode . pack $ printf constantIntegerTemplateSI name val
  | val < 0 && val >= fromIntegral (minBound @Int.Int32) =
    CCode . pack $ printf constantIntegerTemplateUI name val
  | otherwise =
    CCode . pack $ printf constantIntegerTemplateChar name val


constantIntegerTemplateSI :: String
constantIntegerTemplateSI =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = IntegerType;
  mpz_init(result->value.integer.mpz);
  mpz_set_si(result->value.integer.mpz, %d);

  return result;
}
  |]

constantIntegerTemplateUI :: String
constantIntegerTemplateUI =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = IntegerType;
  mpz_init(result->value.integer.mpz);
  mpz_set_ui(result->value.integer.mpz, %d);

  return result;
}
  |]


constantIntegerTemplateChar :: String
constantIntegerTemplateChar =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = IntegerType;
  mpz_init(result->value.integer.mpz);
  mpz_set_str(result->value.integer.mpz, "%d", 10);

  return result;
}
  |]

compileConstantBool :: CName -> Bool -> CCode
compileConstantBool (CName name) val =
  CCode . pack $ printf constantBoolTemplate name (if val then 1 else 0 :: Integer)


constantBoolTemplate :: String
constantBoolTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = BoolType;
  result->value.boolean = %d;
  return result;
}
  |]

compileConstantUnit :: CName  -> CCode
compileConstantUnit (CName name) =
  CCode . pack $ printf constantUnitTemplate name


constantUnitTemplate :: String
constantUnitTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = UnitType;
  return result;
}
  |]

compileConstantByteString :: CName -> BS.ByteString -> CCode
compileConstantByteString (CName name) bs =
  CCode . pack $ printf constantByteStringTemplate name asCInitializer (BS.length bs)
  where
    asCInitializer :: String
    asCInitializer = concat $ intersperse "," $ show . toInteger <$> toList bs



constantByteStringTemplate :: String
constantByteStringTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  static const unsigned char table[] = {%s};
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = ByteStringType;
  result->value.byteString.length = %d;
  result->value.byteString.bytes = &(table[0]);
  return result;
}
  |]


compileConstantString :: CName -> Text -> CCode
compileConstantString (CName name) text =
  CCode . pack $ printf constantStringTemplate name asCInitializer (BS.length bs)
  where
    bs :: BS.ByteString
    bs = encodeUtf8 text

    asCInitializer :: String
    asCInitializer = concat $ intersperse "," $ show . toInteger <$> toList bs



constantStringTemplate :: String
constantStringTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  static const unsigned char table[] = {%s};
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = TextType;
  result->value.byteString.length = %d;
  result->value.byteString.bytes = &(table[0]);
  return result;
}
  |]

compileConstantPair :: CName -> CName -> CName -> CCode
compileConstantPair (CName name) (CName x) (CName y) =
  CCode . pack $ printf constantPairTemplate name x y



constantPairTemplate :: String
constantPairTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  const struct NFData *fst = %s(scope);
  const struct NFData *snd = %s(scope);

  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = PairType;
  result->value.pair.fst = fst;
  result->value.pair.snd = snd;
  return result;
}
  |]


compileConstantList :: CName -> [CName] -> CCode
compileConstantList (CName name) names =
  CCode . pack $ printf constantListTemplate1 name nodes
  where
    nodes :: Text
    nodes = fold $ pack . printf constantListTemplate2 . unCName <$> reverse names


constantListTemplate1 :: String
constantListTemplate1 =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct List *current = 0;
  struct List *next = 0;
%s
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));
  result->type = ListType;
  result->value.list = current;

  return result;
}
  |]

constantListTemplate2 :: String
constantListTemplate2 =
  [r|
  current = (struct List *)alloc(sizeof(struct List));
  current->elem = %s(scope);
  current->next = next;
  next = current;
  |]


compileDataConstr :: CName -> CName -> CName -> CCode
compileDataConstr (CName name) (CName integer) (CName list) =
  CCode . pack $ printf constantDataConstrTemplate name integer list


constantDataConstrTemplate :: String
constantDataConstrTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));

  result->type = DataType;
  result->value.data.sort = ConstrS;
  result->value.data.value.constr.integer = %s;
  result->value.data.value.constr.list = %s;

  return result;
}
  |]


compileDataMap :: CName -> CName -> CCode
compileDataMap (CName name)(CName pairList) =
  CCode . pack $ printf constantDataMapTemplate name pairList


constantDataMapTemplate :: String
constantDataMapTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));

  result->type = DataType;
  result->value.data.sort = MapS;
  result->value.data.value.map.pairList = %s;

  return result;
}
  |]


compileDataList :: CName -> CName -> CCode
compileDataList (CName name)(CName pairList) =
  CCode . pack $ printf constantDataListTemplate name pairList


constantDataListTemplate :: String
constantDataListTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));

  result->type = DataType;
  result->value.data.sort = ListS;
  result->value.data.value.list.list = %s;

  return result;
}
  |]



compileDataInteger :: CName -> CName -> CCode
compileDataInteger (CName name)(CName pairList) =
  CCode . pack $ printf constantDataIntegerTemplate name pairList


constantDataIntegerTemplate :: String
constantDataIntegerTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));

  result->type = DataType;
  result->value.data.sort = IntegerS;
  result->value.data.value.integer = %s;

  return result;
}
  |]


compileDataByteString :: CName -> CName -> CCode
compileDataByteString (CName name)(CName pairList) =
  CCode . pack $ printf constantDataByteStringTemplate name pairList


constantDataByteStringTemplate :: String
constantDataByteStringTemplate =
  [r|
const struct NFData *%s (const struct LexicalScope *scope) {
  struct NFData *result = (struct NFData *)alloc(sizeof(struct NFData));

  result->type = DataType;
  result->value.data.sort = ByteStringS;
  result->value.data.value.integer = %s;

  return result;
}
  |]
