{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module UPLC2C.Serialization (serialize, deserialize) where

import           Data.Bifunctor  (Bifunctor (first))
import           Data.Bits       (Bits (shiftL, shiftR, (.&.)))
import qualified Data.ByteString as BS
import           Data.Word       (Word32, Word8)
import           GHC.Exts        (IsList (fromList), toList)
import           PlutusCore.Data

constrType :: Word32
constrType = 0

mapType :: Word32
mapType = 1

listType :: Word32
listType = 2

integerType :: Word32
integerType = 3

byteStringType :: Word32
byteStringType = 4

serialize :: Data -> [Word32]
serialize = \case
    Constr i l -> constrType : (serializeInteger i ++ serializeList l)
    Map l      -> mapType : serializeList ((\(x, y) -> [x, y]) =<< l)
    List l     -> listType : serializeList l
    I i        -> integerType : serializeInteger i
    B bs       -> byteStringType : serializeByteString (toList bs)

serializeList :: [Data] -> [Word32]
serializeList l = fromIntegral (length l) : concat (serialize <$> l)

serializeInteger :: Integer -> [Word32]
serializeInteger i = fromIntegral (length s) : sgn : s
  where
    s = serializeNatural abs'
    sgn = if i < 0 then 1 else 0
    abs' = abs i

serializeNatural :: Integer -> [Word32]
serializeNatural i =
    if i == 0
      then []
      else lsw : serializeNatural (i `shiftR` 32)
  where
    lsw = fromIntegral (i .&. 0xffffffff)

group :: Int -> [a] -> [[a]]
group n l =
  let (take', drop') = splitAt n l
   in case (take', drop') of
     ([], _) -> []
     (_, []) -> [take']
     (_, _)  -> take' : group n drop'

serializeByteString :: [Word8] -> [Word32]
serializeByteString l = fromIntegral (length l) : words'
  where
    words' = (\l' -> sum $ uncurry shiftL <$> zip l' shifts) <$> grouped
    grouped = group 4 (fromIntegral <$> l)
    shifts = [0,8..]

----

deserialize :: [Word32] -> Maybe (Data, [Word32])
deserialize []     = Nothing
deserialize (x:xs) = case x of
    _ | x == constrType  -> do
        (i, rest) <- deserializeInteger xs
        (d, rest') <- deserializeList rest
        return (Constr i d, rest')
    _ | x == integerType -> do
        (i, rest) <- deserializeInteger xs
        return (I i, rest)
    _ | x == listType -> do
        (l, rest) <- deserializeList xs
        return (List l, rest)
    _ | x == mapType -> do
        (l, rest) <- deserializeList xs
        if even (length l)
          then return (Map $ toPairList l, rest)
          else Nothing
    _ | x == byteStringType -> do
        (bs, rest) <- deserializeByteString xs
        return (B bs, rest)
    _           -> Nothing

toPairList :: [a] -> [(a, a)]
toPairList (x:y:rest) = (x, y) : toPairList rest
toPairList []         = []
toPairList _          = error "Expected even list length"

deserializeInteger :: [Word32] -> Maybe (Integer, [Word32])
deserializeInteger l = case l of
  len:sgn:s | sgn == 0 || sgn == 1 ->
    let lenI = fromIntegral len
        (take', drop') = splitAt lenI s
        sgn' = if sgn == 1 then -1 else 1
     in
    if length s < lenI
      then Nothing
      else return (sgn' * deserializeNatural take', drop')
  _                                -> Nothing

deserializeNatural :: [Word32] -> Integer
deserializeNatural l =
     sum $ uncurry shiftL <$> zip (fromIntegral <$> l) shifts
  where
    shifts :: [Int]
    shifts = [0,32..]

deserializeList :: [Word32] -> Maybe ([Data], [Word32])
deserializeList []            = Nothing
deserializeList (len:payload) = deserializeList' (fromIntegral len) payload

deserializeList' :: Integer -> [Word32] -> Maybe ([Data], [Word32])
deserializeList' 0 payload = Just ([], payload)
deserializeList' len payload = do
  (x, rest) <- deserialize payload
  (xs, rest') <- deserializeList' (len - 1) rest
  return (x:xs, rest')

expand :: Integer -> Word32 -> [Word8]
expand 0 _ = []
expand n w = fromIntegral (w .&. 0xff) : expand (n - 1) (w `shiftR` 8)

deserializeByteString :: [Word32] -> Maybe (BS.ByteString, [Word32])
deserializeByteString (len:payload) = first fromList <$> deserializeByteString' (fromIntegral len) payload
deserializeByteString _ = Nothing

deserializeByteString' :: Integer -> [Word32] -> Maybe ([Word8], [Word32])
deserializeByteString' 0 payload  = Just ([], payload)
deserializeByteString' _ []       = Nothing
deserializeByteString' n (b:rest) =
  if n > 4
    then do
      let f = expand 4 b
      (bs, rest') <- deserializeByteString' (n - 4) rest
      Just (f ++ bs, rest')
    else Just (expand n b, rest)

