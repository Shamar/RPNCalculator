-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Giacomo Tesio
-- License     :  GNU General Public License Version 3
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
  putStrLn $ format $ evaluate (floatingRPN) args

format :: (Show a) => Either String a -> String
format (Left e) = e
format (Right result) = show result

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
