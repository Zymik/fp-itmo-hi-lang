{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HW3.Evaluator
  ( eval
  )
where

import Codec.Compression.Zlib
  ( CompressParams (compressLevel)
  , bestCompression
  , compressWith
  , decompressWith
  , defaultCompressParams
  , defaultDecompressParams
  )
import Codec.Serialise (deserialise, serialise)
import Control.Applicative ((<|>))
import Control.Applicative.Combinators (choice)
import Control.Lens (review)
import Control.Lens.Combinators
  ( preview
  )
import Control.Monad (foldM)
import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Semigroup (stimes)
import Data.Sequence (Seq ((:<|)), (><))
import qualified Data.Sequence as S
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (diffUTCTime)
import Data.Time.Clock (addUTCTime)
import Data.Word (Word8)
import GHC.Exts (IsList (Item, fromList, toList))
import HW3.Base
import HW3.ValueInner
import Text.Read (readMaybe)

-- main eval
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr =
  do
    res <- runExceptT $ evalExpect expr
    return $ case res of
      Left hew -> Left $ getHiError hew
      Right hv -> Right hv

type ExpectEval m a = ExceptT HiEvalError m a

-- Wrapper for errors that can be combined by semigroup operations
newtype HiEvalError = HiErrorWrapped {getHiError :: HiError}

instance Semigroup HiEvalError where
  (<>) :: HiEvalError -> HiEvalError -> HiEvalError
  (<>) a b = case getHiError a of
    HiErrorInvalidArgument -> b
    _ -> a

instance Monoid HiEvalError where
  mempty :: HiEvalError
  mempty = HiErrorWrapped HiErrorInvalidArgument

throwHi :: HiMonad m => HiError -> ExpectEval m a
throwHi e = throwError $ HiErrorWrapped e

-- eval in monad
evalExpect :: HiMonad m => HiExpr -> ExpectEval m HiValue
evalExpect = \case
  HiExprValue value -> return value
  HiExprApply expr args ->
    do
      val <- evalExpect expr
      evalApply val args
  HiExprRun expr ->
    do
      val <- evalExpect expr
      case val of
        HiValueAction action -> lift $ runAction action
        _ -> throwHi HiErrorInvalidArgument
  HiExprDict dict ->
    let values = do
          (key, value) <- dict
          return $ (,) <$> evalExpect key <*> evalExpect value
     in do
          pairs <- sequence values
          returnHi @(Map HiValue HiValue) $ fromList pairs

-- Eval applying
evalApply :: HiMonad m => HiValue -> [HiExpr] -> ExpectEval m HiValue
evalApply val args =
  case val of
    HiValueFunction hf -> evalFunWithArgs hf args
    HiValueString str  -> evalStringWithArgs str args
    HiValueList list   -> evalListWithArgs list args
    HiValueBytes bytes -> evalBytesWithArgs bytes args
    HiValueDict dict   -> evalDictWithArgs dict args
    _                  -> throwHi HiErrorInvalidFunction

-- functions with different arity
evalFunWithArgs :: HiMonad m => HiFun -> [HiExpr] -> ExpectEval m HiValue
evalFunWithArgs HiFunList list =
  do
    args <- traverse evalExpect list
    return $ HiValueList $ fromList args

evalFunWithArgs fun [a] = evalUnaryFun fun a
evalFunWithArgs fun [a, b] = evalBinaryFun fun a b
evalFunWithArgs fun [a, b, c] = evalTernaryFun fun a b c
evalFunWithArgs _ _ = throwHi HiErrorArityMismatch

-- string operations
evalStringWithArgs :: HiMonad m => Text -> [HiExpr] -> ExpectEval m HiValue
evalStringWithArgs str [a] = evalIndex T.length T.index str a
evalStringWithArgs str [a, b] = evalSlice str a b
evalStringWithArgs _ _ = throwHi HiErrorArityMismatch

-- list operations
evalListWithArgs :: HiMonad m => Seq HiValue -> [HiExpr] -> ExpectEval m HiValue
evalListWithArgs s [a] = evalIndex S.length S.index s a
evalListWithArgs s [a, b] = evalSlice s a b
evalListWithArgs _ _ = throwHi HiErrorArityMismatch

-- bytes operations
evalBytesWithArgs :: HiMonad m => ByteString -> [HiExpr] -> ExpectEval m HiValue
evalBytesWithArgs bytes [a] = evalIndex B.length B.index bytes a
evalBytesWithArgs bytes [a, b] = evalSlice bytes a b
evalBytesWithArgs _ _ = throwHi HiErrorArityMismatch

-- dict operations
evalDictWithArgs :: HiMonad m => Map HiValue HiValue -> [HiExpr] -> ExpectEval m HiValue
evalDictWithArgs dict [expr] = unaryHiFun (\val -> fromMaybe HiValueNull $ M.lookup val dict) expr
evalDictWithArgs _ _ = throwHi HiErrorArityMismatch

-- unary funcs
evalUnaryFun :: HiMonad m => HiFun -> HiExpr -> ExpectEval m HiValue
evalUnaryFun = \case
  HiFunNot -> unaryHiFun not

  HiFunLength ->
    overloadUnary
      [ unaryHi $ S.length @HiValue
      , unaryHi T.length
      , unaryHi B.length
      ]

  HiFunReverse ->
    overloadUnary
      [ unaryHi $ S.reverse @HiValue
      , unaryHi T.reverse
      , unaryHi B.reverse
      ]

  HiFunToUpper -> unaryHiFun T.toUpper

  HiFunToLower -> unaryHiFun T.toLower

  HiFunTrim -> unaryHiFun T.strip

  HiFunPackBytes -> unaryHiFun (B.pack . (toList @(Seq Word8)))

  HiFunUnpackBytes -> unaryHiFun \val -> fromList @(Seq Integer) (toInteger <$> unpack val)

  HiFunDecodeUtf8 ->
    unaryFun
      \val ->
        case decodeUtf8' val of
          Left _ -> returnNull
          Right txt -> returnHi txt

  HiFunEncodeUtf8 -> unaryHiFun encodeUtf8

  HiFunZip -> unaryHiFun compress

  HiFunUnzip -> unaryHiFun decompress

  HiFunSerialise -> unaryHiFun (toStrict . serialise @HiValue)

  HiFunDeserialise -> unaryHiFun (deserialise @HiValue . fromStrict)

  HiFunRead -> unaryHiFun (HiActionRead . T.unpack)

  HiFunMkDir -> unaryHiFun (HiActionMkDir . T.unpack)

  HiFunChDir -> unaryHiFun (HiActionChDir . T.unpack)

  HiFunParseTime -> unaryHiFun (\val -> maybe HiValueNull HiValueTime (readMaybe $ T.unpack val))

  HiFunEcho -> unaryHiFun HiActionEcho

  HiFunKeys ->
    unaryHiFun
      (\(val :: Map HiValue HiValue) -> fromList @(Seq HiValue) $ M.keys val)

  HiFunValues -> unaryHiFun
    \(val :: Map HiValue HiValue) -> fromList @(Seq HiValue) $ M.elems val

  HiFunInvert -> unaryHiFun
    \(val :: Map HiValue HiValue) ->
      let dict = M.fromListWith (><) $ zip (M.elems val) (M.keys val & map S.singleton)
       in M.map HiValueList dict

  HiFunCount ->
    overloadUnary
      [ unaryHi $ evalCount @(Seq HiValue)
      , unaryHi $ evalCount @Text
      , unaryHi $ evalCount @ByteString
      ]
    where
      evalCount :: (IsList a, HiValueInner (Item a)) => a -> Map HiValue HiValue
      evalCount isList =
        let list = toList isList & map (review valuePrism)
            dict = M.fromListWith (+) $ zip list (repeat 1)
         in M.map HiValueNumber dict

-- arity missmatch
  _ -> \_ -> throwHi HiErrorArityMismatch

-- compress and decompress functions
compressParams :: CompressParams
compressParams = defaultCompressParams {compressLevel = bestCompression}

compress :: ByteString -> ByteString
compress = toStrict . compressWith compressParams . fromStrict

decompress :: ByteString -> ByteString
decompress = toStrict . decompressWith defaultDecompressParams . fromStrict

-- Binary funcs
evalBinaryFun :: HiMonad m => HiFun -> HiExpr -> HiExpr -> ExpectEval m HiValue
evalBinaryFun = \case
  HiFunAdd ->
    overloadBinary
      [ binaryHi $ (+) @Rational
      , binaryHi $ (<>) @ByteString
      , binaryHi $ (<>) @Text
      , binaryHi $ (<>) @(Seq HiValue)
      , binaryHi \time rat -> addUTCTime (fromRational rat) time
      ]

  HiFunSub ->
    overloadBinary
      [ binaryHi $ (-) @Rational
      , binaryHi \a b -> toRational $ diffUTCTime a b
      ]

  HiFunMul ->
    overloadBinary
      [ binaryHi $ (*) @Rational
      , binary $ times @Text
      , binary $ times @(Seq HiValue)
      , binary $ times @ByteString
      ]
    where
      times :: (HiValueInner s, Semigroup s, HiMonad m) => s -> Integer -> ExpectEval m HiValue
      times s val =
        if val > 0
          then returnHi (stimes val s)
          else throwHi HiErrorInvalidArgument

  HiFunDiv ->
    overloadBinary
      [ binary \(a :: Rational) (b :: Rational) ->
          if b == 0
            then throwHi HiErrorDivideByZero
            else returnHi $ a / b
      , binaryHi \s1 s2 -> T.concat [s1, fromString "/", s2]
      ]

  HiFunEquals -> binaryHiFun ((==) @HiValue)

  HiFunNotEquals -> binaryHiFun ((/=) @HiValue)

  HiFunLessThan -> binaryHiFun ((<) @HiValue)

  HiFunNotLessThan -> binaryHiFun ((>=) @HiValue)

  HiFunGreaterThan -> binaryHiFun ((>) @HiValue)

  HiFunNotGreaterThan -> binaryHiFun ((<=) @HiValue)

  HiFunFold ->
    binaryFun
      \fun s ->
        case s of
          h :<| t -> foldM (\a b -> evalApply fun [HiExprValue a, HiExprValue b]) h t
          _ -> returnNull

  HiFunAnd -> \l r ->
    do
      left <- evalExpect l
      if left == HiValueBool False || left == HiValueNull
        then return left
        else evalExpect r

  HiFunOr -> \l r ->
    do
      left <- evalExpect l
      if left == HiValueBool False || left == HiValueNull
        then evalExpect r
        else return left

  HiFunRange ->
    binaryHiFun
      \(left :: Rational) (right :: Rational) ->
        let s = HiValueNumber <$> [left .. right]
         in fromList @(Seq HiValue) s

  HiFunWrite ->
    overloadBinary
      [ binaryHi \file txt -> HiActionWrite (T.unpack file) (encodeUtf8 txt)
      , binaryHi $ HiActionWrite . T.unpack
      ]

  HiFunRand ->
    binaryFun
      \left right ->
        if left > right
          then throwHi HiErrorInvalidArgument
          else returnHi $ HiActionRand left right

  _ -> \_ _ -> throwHi HiErrorArityMismatch

-- Indexes
type LengthGetter a = a -> Int

type GetByIndex a b = a -> Int -> b

evalIndex ::
  (HiMonad m, HiValueInner (Item a)) =>
  LengthGetter a ->
  GetByIndex a (Item a) ->
  a ->
  HiExpr ->
  ExpectEval m HiValue
evalIndex lengther indexer iter expr =
  do
    i <- evalHi expr
    if i < 0 || i >= toInteger (lengther iter)
      then returnNull
      else returnHi $ indexer iter (fromIntegral i)

-- Slices
evalSlice :: (HiMonad m, Sliceable a, HiValueInner a) => a -> HiExpr -> HiExpr -> ExpectEval m HiValue
evalSlice (iter :: a) l r =
  do
    let len = size iter
    let indexer = evalSliceIndex len
    left <- indexer l 0
    right <- indexer r (toInteger len)
    let sliced = slice left right iter
    returnHi sliced

evalSliceIndex :: HiMonad m => Int -> HiExpr -> Integer -> ExpectEval m Int
evalSliceIndex len expr def =
  do
    i <- evalHi expr <|> return def
    return $ getIndexWithLen len i

getIndexWithLen :: Int -> Integer -> Int
getIndexWithLen len i
  | i > toInteger len = len
  | i < 0 && abs i >= toInteger len = 0
  | i < 0 = len + fromIntegral i
  | otherwise = fromIntegral i

-- Ternary func
evalTernaryFun :: HiMonad m => HiFun -> HiExpr -> HiExpr -> HiExpr -> ExpectEval m HiValue
evalTernaryFun HiFunIf cond left right =
  do
    boolCond <- evalHi cond
    if boolCond
      then evalExpect left
      else evalExpect right
evalTernaryFun _ _ _ _ = throwHi HiErrorArityMismatch

returnNull :: Monad m => m HiValue
returnNull = return HiValueNull

evalHi :: (HiMonad m, HiValueInner a) => HiExpr -> ExpectEval m a
evalHi expr = do
  val <- evalExpect expr
  case preview valuePrism val of
    Nothing -> throwError mempty
    Just a -> return a

previewHi :: (HiMonad m, HiValueInner a) => HiValue -> ExpectEval m a
previewHi val = case preview valuePrism val of
  Nothing -> throwError mempty
  Just a -> return a

-- binary functions from inner values functions
binary ::
  (HiValueInner a, HiValueInner b, HiMonad m) =>
  (a -> b -> ExpectEval m c) ->
  HiValue ->
  HiValue ->
  ExpectEval m c
binary f left right =
  do
    a <- previewHi left
    b <- previewHi right
    f a b

binaryHi ::
  (HiValueInner a, HiValueInner b, HiValueInner c, HiMonad m) =>
  (a -> b -> c) ->
  HiValue ->
  HiValue ->
  ExpectEval m HiValue
binaryHi f = binary (\a b -> returnHi $ f a b)

-- overloads for binary hi value functions
overloadBinary :: HiMonad m => [HiValue -> HiValue -> ExpectEval m c] -> HiExpr -> HiExpr -> ExpectEval m c
overloadBinary functions l r =
  do
    left <- evalExpect l
    right <- evalExpect r
    let results = map (\s -> s left right) functions
    choice results

-- binary functions with one overload
binaryFun ::
  (HiValueInner a, HiValueInner b, HiMonad m) =>
  (a -> b -> ExpectEval m c) ->
  HiExpr ->
  HiExpr ->
  ExpectEval m c
binaryFun f = overloadBinary [binary f]

binaryHiFun ::
  (HiValueInner a, HiValueInner b, HiValueInner c, HiMonad m) =>
  (a -> b -> c) ->
  HiExpr ->
  HiExpr ->
  ExpectEval m HiValue
binaryHiFun f = binaryFun $ binaryHi f

-- unary functions from inner values functions
unary ::
  (HiValueInner a, HiMonad m) =>
  (a -> ExpectEval m c) ->
  HiValue ->
  ExpectEval m c
unary f v = do
  value <- previewHi v
  f value

unaryHi ::
  (HiValueInner a, HiValueInner b, HiMonad m) =>
  (a -> b) ->
  HiValue ->
  ExpectEval m HiValue
unaryHi f = unary (returnHi . f)

-- overloads for unary hi value functions
overloadUnary :: HiMonad m => [HiValue -> ExpectEval m c] -> HiExpr -> ExpectEval m c
overloadUnary functions v =
  do
    value <- evalExpect v
    let results = map (\s -> s value) functions
    choice results

-- unary functions with one overload
unaryFun ::
  (HiValueInner a, HiMonad m) =>
  (a -> ExpectEval m c) ->
  HiExpr ->
  ExpectEval m c
unaryFun f = overloadUnary [unary f]

unaryHiFun ::
  (HiValueInner a, HiValueInner b, HiMonad m) =>
  (a -> b) ->
  HiExpr ->
  ExpectEval m HiValue
unaryHiFun f = unaryFun $ unaryHi f

class Sliceable s where
  
  size :: s -> Int

  takeItems ::  Int -> s -> s

  dropItems :: Int -> s -> s

  slice :: Int -> Int -> s -> s
  slice begin end = takeItems (end - begin) . dropItems begin

instance Sliceable (Seq HiValue) where
  size = S.length

  takeItems = S.take

  dropItems = S.drop

instance Sliceable Text where
  size = T.length

  takeItems = T.take

  dropItems = T.drop

instance Sliceable ByteString where
  size = B.length

  takeItems = B.take

  dropItems = B.drop
