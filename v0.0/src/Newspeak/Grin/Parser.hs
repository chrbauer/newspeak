module Newspeak.Grin.Parser (parseExpr) where

import Data.Char (isSpace, isDigit)
import Text.Read (readMaybe)
import Newspeak.Grin.Ast

parseExpr :: String -> Either String GExpr
parseExpr s =
  case splitPlus (strip s) of
    Just (a,b) -> do
      x <- parseInt a
      y <- parseInt b
      pure (GAdd (GLit x) (GLit y))
    Nothing -> Left "parse error: expected A+B with integer literals"
  where
    strip = f . f where f = reverse . dropWhile isSpace
    parseInt t =
      case readMaybe t :: Maybe Int of
        Just n  -> Right n
        Nothing -> Left ("parse error: bad int: " ++ show t)
    splitPlus t =
      case break (== '+') t of
        (l,'+':r) | not (null l) && not (null r) -> Just (strip l, strip r)
        _ -> Nothing
   
