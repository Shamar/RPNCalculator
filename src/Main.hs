-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Giacomo Tesio
-- License     :  AllRightsReserved
--
-- Maintainer  :  Giacomo Tesio
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
main
) where

import System.Environment( getArgs )
import Data.Maybe (isNothing)
import Computation


main = do
  args <- getArgs
  let result = evaluate (floatingRPN) (words "5")
  print $ show $ result


calculate = evaluate floatingRPN


floatingRPN :: (Floating a, Read a) => String -> Computation a
floatingRPN "+" = binary (+)
floatingRPN "-" = binary (-)
floatingRPN "*" = binary (*)
floatingRPN "/" = binary (/)
floatingRPN "^" = binary (**)
floatingRPN "negate" = unary negate
floatingRPN "sqrt" = unary sqrt
floatingRPN token = readValue token

readValue :: (Read a, Show a) => String -> Computation a
readValue token = extract parsed
    where extract [(x, "")] = value x
          extract _ = parseError $ "Can not parse: " ++ token ++ "."
          parsed = reads token
