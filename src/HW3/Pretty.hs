{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module HW3.Pretty
  ( prettyValue
  )
where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Ratio
import Data.Scientific
import Data.Sequence (Seq (..))
import Data.Word (Word8)
import GHC.Exts (toList)
import HW3.Base (HiAction (..), HiValue (..), hiFunKeyword)
import Numeric (showFFloat, showHex)
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , dquotes
  , viaShow
  , (<+>)
  , annotate
  )
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, italicized)

constStyle :: Doc AnsiStyle -> Doc AnsiStyle
constStyle = annotate $ color Green

stringStyle :: Doc AnsiStyle -> Doc AnsiStyle
stringStyle = annotate $ color Magenta <> italicized

prettyString :: (Show a) => a -> Doc AnsiStyle
prettyString = stringStyle . viaShow

numberStyle :: Doc AnsiStyle -> Doc AnsiStyle
numberStyle = annotate $ color Yellow

bracketStyle :: Doc AnsiStyle -> Doc AnsiStyle
bracketStyle = annotate $ color Blue

funcStyle :: Doc AnsiStyle -> Doc AnsiStyle
funcStyle = annotate $ color Cyan

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber rat)
  | den == 1 = numberStyle $ pretty num
  | decimalDenominator den = numberStyle $ 
    (pretty @String) $ showFFloat Nothing (toRealFloat @Double $ fst $ fromRationalRepetendUnlimited rat) ""
  | otherwise = numberStyle $ prettyFraction (quotRem num den) den
  where
    num = numerator rat
    den = denominator rat

prettyValue (HiValueFunction hf) = funcStyle $ pretty $ hiFunKeyword hf
prettyValue (HiValueBool True) = constStyle $ pretty "true"
prettyValue (HiValueBool False) = constStyle $ pretty "false"
prettyValue (HiValueString str) = prettyString str
prettyValue HiValueNull = constStyle $ pretty "null"
prettyValue (HiValueList l) = prettyList l
prettyValue (HiValueBytes bytes) = prettyBytes bytes
prettyValue (HiValueDict dict) = prettyDict dict
prettyValue (HiValueAction action) = prettyAction action
prettyValue (HiValueTime t) = funcStyle (pretty "parse-time") 
                            <> bracketStyle (pretty "(") 
                            <> stringStyle (dquotes (viaShow t)) 
                            <> bracketStyle (pretty ")")

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes bytes = prettyFromList "[#" (prettyHex <$> toList bytes) "#]" ""

prettyList :: Seq HiValue -> Doc AnsiStyle
prettyList s = prettyFromList "[" (prettyValue <$> toList s) "]" ","

prettyDict :: Map HiValue HiValue -> Doc AnsiStyle
prettyDict dict = prettyFromList "{" (prettyMapPair <$> toList dict) "}" ","

prettyFromList :: String -> [Doc AnsiStyle] -> String -> String -> Doc AnsiStyle
prettyFromList open (prettyHead : prettyTail) close s =
    bracketStyle (pretty open)
    <+> foldl (\a b -> a <> pretty s <+> b) prettyHead prettyTail
    <+> bracketStyle (pretty close)

prettyFromList open _ close _ = bracketStyle (pretty open) <+> bracketStyle (pretty close)


prettyHex :: Word8 -> Doc AnsiStyle
prettyHex w8 =
  numberStyle $ 
  pretty $ case showHex w8 "" of
    [a] -> ['0', a]
    s -> s

-- Numbers pretty
decimalDenominator :: Integer -> Bool
decimalDenominator x
  | x == 1 = True
  | even x = decimalDenominator (div x 2)
  | mod x 5 == 0 = decimalDenominator (div x 5)
  | otherwise = False

prettyProperFraction :: Integer -> Integer -> Doc AnsiStyle
prettyProperFraction num den = pretty num <> pretty "/" <> pretty den

prettyFraction :: (Integer, Integer) -> Integer -> Doc AnsiStyle
prettyFraction (intPart, num) den
  | intPart == 0 = prettyProperFraction num den
  | num < 0 = prettyMixedFraction intPart (abs num) den "-"
  | otherwise = prettyMixedFraction intPart num den "+"

prettyMixedFraction :: Integer -> Integer -> Integer -> String -> Doc AnsiStyle
prettyMixedFraction int den num sign = pretty int <+> pretty sign <+> prettyProperFraction den num

prettyMapPair :: (HiValue, HiValue) -> Doc AnsiStyle
prettyMapPair (key, value) = prettyValue key <> pretty ":" <+> prettyValue value

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction = \case
  HiActionRead s     -> prettyActionWithArgs "read" [prettyString s]
  HiActionWrite s bs -> prettyActionWithArgs "write" [prettyString s, prettyBytes bs]
  HiActionMkDir s    -> prettyActionWithArgs "mkdir" [prettyString s]
  HiActionChDir s    -> prettyActionWithArgs "cd" [prettyString s]
  HiActionCwd        -> funcStyle $ pretty "cwd"
  HiActionNow        -> funcStyle $ pretty "now"
  HiActionRand n i   -> prettyActionWithArgs "rand"  $ numberStyle . pretty<$> [n,  i]
  HiActionEcho txt   -> prettyActionWithArgs "echo" [prettyString txt]

prettyActionWithArgs :: String -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyActionWithArgs action [] = funcStyle $ pretty action
prettyActionWithArgs action l =  funcStyle (pretty action) 
                              <> bracketStyle (pretty "(") 
                              <> prettyWithSeparator l "," 
                              <> bracketStyle (pretty ")")
                                where 
                                    prettyWithSeparator :: [Doc AnsiStyle] -> String -> Doc AnsiStyle
                                    prettyWithSeparator (h : t) s  = foldl (\a b -> a <> pretty s <+> b) h t
                                    prettyWithSeparator _ _        = pretty ""
