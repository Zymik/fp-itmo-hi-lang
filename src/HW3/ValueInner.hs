{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module HW3.ValueInner
  ( HiValueInner (..)
  )
where

import Control.Lens (Prism', preview, prism')
import Control.Lens.Combinators (review)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Ratio (denominator)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Word (Word8)
import GHC.Real (numerator)
import HW3.Base

getInteger :: Prism' Rational Integer
getInteger = prism' toRational $ \i ->
  if denominator i == 1
    then Just $ numerator i
    else Nothing

getInt :: Prism' Integer Int
getInt = prism' toInteger $ \i ->
  if toInteger (minBound @Int) <= i && i <= toInteger (maxBound @Int)
    then Just $ fromInteger i
    else Nothing

getTextChar :: Prism' Text Char
getTextChar = prism' T.singleton $ \i ->
  if T.length i == 1
    then Just $ T.head i
    else Nothing

getWord8 :: Prism' Integer Word8
getWord8 = prism' toInteger (\x -> if x >= 0 && x <= 255 then Just $ fromInteger x else Nothing)

-- HiValueInner, used for types that can be wrapped/unwrapped in HiValue
class HiValueInner a where
  valuePrism :: Prism' HiValue a

  returnHi :: Monad m => a -> m HiValue
  returnHi val = return $ review valuePrism val

-- Instances for different types
instance HiValueInner HiValue where
  valuePrism = prism' id Just

instance HiValueInner Rational where
  valuePrism = _HiValueNumber

instance HiValueInner Bool where
  valuePrism = _HiValueBool

instance HiValueInner HiFun where valuePrism = _HiValueFunction

instance HiValueInner Text where
  valuePrism = _HiValueString

instance HiValueInner ByteString where
  valuePrism = _HiValueBytes

instance HiValueInner UTCTime where
  valuePrism = _HiValueTime

instance HiValueInner HiAction where
  valuePrism = _HiValueAction

instance HiValueInner Integer where
  valuePrism = valuePrism . getInteger

instance HiValueInner Word8 where
  valuePrism = valuePrism . getWord8

instance HiValueInner (Map HiValue HiValue) where
  valuePrism = _HiValueDict

instance (HiValueInner a) => HiValueInner (Seq a) where
  valuePrism = getSeqPrism valuePrism

instance HiValueInner Int where
  valuePrism = valuePrism . getInt

instance HiValueInner Char where
  valuePrism = valuePrism . getTextChar

getSeqPrism :: Prism' HiValue a -> Prism' HiValue (Seq a)
getSeqPrism pr =
  prism'
    (\s -> HiValueList $ review pr <$> s)
    ( \v ->
        do
          s <- preview _HiValueList v
          traverse (preview pr) s
    )
