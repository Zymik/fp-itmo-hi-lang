module HW3.Parser
  ( parse
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlphaNum)
import Data.Function
import Data.Functor (($>))
import Data.List (intercalate)
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import Data.Void
import Data.Word (Word8)
import GHC.Exts (fromList)
import GHC.Unicode (isAlpha)
import HW3.Base
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
  ( char
  , hexDigitChar
  , space
  , space1
  , string
  )
import Text.Megaparsec.Char.Lexer (scientific, signed)
import qualified Text.Megaparsec.Char.Lexer as L

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> parseHiExprWithOp <* eof) ""

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

funParser :: [Parser HiFun]
funParser = allHiFunc & map (\x -> string (hiFunKeyword x) $> x)

parseFun :: Parser HiValue
parseFun =
  HiValueFunction
    <$> choice funParser

parseHiNumber :: Parser HiValue
parseHiNumber = signed space scientific & fmap (HiValueNumber . toRational)

parseHiBool :: Parser HiValue
parseHiBool =
  HiValueBool
    <$> choice
      [ string "true" $> True
      , string "false" $> False
      ]

parseHiNull :: Parser HiValue
parseHiNull = string "null" $> HiValueNull

parseHiCwd :: Parser HiValue
parseHiCwd = string "cwd" $> HiValueAction HiActionCwd

parseHiNow :: Parser HiValue
parseHiNow = string "now" $> HiValueAction HiActionNow

stringLiteral :: Parser Text
stringLiteral = pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

parseString :: Parser HiValue
parseString = HiValueString <$> stringLiteral

parseHiExprWithOp :: Parser HiExpr
parseHiExprWithOp = makeExprParser (lexeme parseHiExpr) tableOp

parseHiExpr :: Parser HiExpr
parseHiExpr = try (parseHiExprMain <|> inRoundBracket parseHiExprWithOp >>= parseHiApply)

parseHiApply :: HiExpr -> Parser HiExpr
parseHiApply a =
  do
    expr <- parseForceOrDot a
    choice
      [ try $ space *> parseHiWithArgs expr
      , return expr
      ]

parseHiExprList :: Parser [HiExpr]
parseHiExprList = sepBy (try parseHiExprWithOp) comma

parseHiList :: Parser HiExpr
parseHiList =
  do
    inner <- inSquareBracket parseHiExprList
    return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) inner

mapPair :: Parser (HiExpr, HiExpr)
mapPair =
  do
    key <- parseHiExprWithOp
    _ <- symbol ":"
    value <- parseHiExprWithOp
    return (key, value)

parseHiExprMap :: Parser [(HiExpr, HiExpr)]
parseHiExprMap = sepBy mapPair comma

parseHiMap :: Parser HiExpr
parseHiMap = HiExprDict <$> inFigureBracket parseHiExprMap

parseHiWithArgs :: HiExpr -> Parser HiExpr
parseHiWithArgs expr =
  do
    inner <- inRoundBracket parseHiExprList
    parseHiApply $ HiExprApply expr inner

parseForceOrDot :: HiExpr -> Parser HiExpr
parseForceOrDot expr =
  choice
    [ try (char '!' $> HiExprRun expr) >>= parseForceOrDot
    , try (char '.' *> parseDot expr) >>= parseForceOrDot
    , return expr
    ]

parseDot :: HiExpr -> Parser HiExpr
parseDot expr =
  do
    arg <- parseId
    return $ HiExprApply expr [arg]

parseId :: Parser HiExpr
parseId = HiExprValue . HiValueString . fromString <$> identity
  where
    identity = intercalate "-" <$> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'

parseHiSingleValue :: Parser HiExpr
parseHiSingleValue =
  HiExprValue
    <$> choice
      [ try parseHiNumber
      , try parseFun
      , try parseHiBool
      , try parseHiNull
      , try parseHiCwd
      , try parseString
      , try parseHiBytes
      , try parseHiNow
      ]

parseHiBytes :: Parser HiValue
parseHiBytes = inByteBracket (HiValueBytes . fromList <$> sepEndBy hexByte space1)

parseHiExprMain :: Parser HiExpr
parseHiExprMain =
  choice
    [ parseHiSingleValue
    , try parseHiList
    , try parseHiMap
    ]

inBracket :: String -> String -> Parser a -> Parser a
inBracket open close = between (symbol open) (string close)

inRoundBracket :: Parser a -> Parser a
inRoundBracket = inBracket "(" ")"

inSquareBracket :: Parser a -> Parser a
inSquareBracket = inBracket "[" "]"

inByteBracket :: Parser a -> Parser a
inByteBracket = inBracket "[#" "#]"

inFigureBracket :: Parser a -> Parser a
inFigureBracket = inBracket "{" "}"

comma :: Parser String
comma = symbol ","

hexByte :: Parser Word8
hexByte =
  do
    l <- hexDigitChar
    r <- hexDigitChar
    return $ read ['0', 'x', l, r]

-- Operators parser definition
binaryOp :: HiFun -> HiExpr -> HiExpr -> HiExpr
binaryOp fun l r = HiExprApply (HiExprValue $ HiValueFunction fun) [l, r]

infixL :: (a -> a -> a) -> String -> Operator Parser a
infixL f name = InfixL (f <$ symbol name)

infixR :: (a -> a -> a) -> String -> Operator Parser a
infixR f name = InfixR (f <$ symbol name)

infixN :: (a -> a -> a) -> String -> Operator Parser a
infixN f name = InfixN (f <$ symbol name)

tableOp :: [[Operator Parser HiExpr]]
tableOp =
  [ [mulOp, divOp]
  , [addOp, subOp]
  , [notGreaterOp, notLessOp, lessOp, greaterOp, equalsOp, notEqualsOp]
  , [andOp]
  , [orOp]
  ]
  where
    mulOp :: Operator Parser HiExpr
    mulOp = infixL (binaryOp HiFunMul) "*"

    divOp :: Operator Parser HiExpr
    divOp = InfixL (binaryOp HiFunDiv <$ try (symbol "/" <* notFollowedBy (symbol "=")))

    addOp :: Operator Parser HiExpr
    addOp = infixL (binaryOp HiFunAdd) "+"

    subOp :: Operator Parser HiExpr
    subOp = infixL (binaryOp HiFunSub) "-"

    lessOp :: Operator Parser HiExpr
    lessOp = infixN (binaryOp HiFunLessThan) "<"

    greaterOp :: Operator Parser HiExpr
    greaterOp = infixN (binaryOp HiFunGreaterThan) ">"

    notLessOp :: Operator Parser HiExpr
    notLessOp = infixN (binaryOp HiFunLessThan) ">="

    notGreaterOp :: Operator Parser HiExpr
    notGreaterOp = infixN (binaryOp HiFunGreaterThan) "<="

    equalsOp :: Operator Parser HiExpr
    equalsOp = infixN (binaryOp HiFunEquals) "=="

    notEqualsOp :: Operator Parser HiExpr
    notEqualsOp = infixN (binaryOp HiFunNotEquals) "/="

    andOp :: Operator Parser HiExpr
    andOp = infixR (binaryOp HiFunAnd) "&&"

    orOp :: Operator Parser HiExpr
    orOp = infixR (binaryOp HiFunOr) "||"
