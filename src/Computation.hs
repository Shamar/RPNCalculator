-----------------------------------------------------------------------------
--
-- Module      :  Computation
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

module Computation (
    Computation,
    value,
    binary,
    unary,
    parseError,
    evaluate
) where

evaluate :: (Show a) =>  (String -> Computation a) -> [String] -> Either String a
evaluate language tokens = compute $ foldr (\s acc -> apply acc $ language s) Empty tokens

data Computation a =
      Empty
    | Value a
    | Function (a -> Computation a)
    | Error String

value x = Value x
binary f = Function $ \x -> unary $ (flip f) x
unary  f = Function (\x -> Value $ f x)
parseError = Error

instance (Show a) => Show (Computation a) where
    show Empty = "Empty"
    show (Value x) = "Value " ++ show x
    show (Function f) = "Function"
    show (Error e) = "Error: " ++ e

apply :: (Show a) => Computation a -> Computation a -> Computation a
apply Empty c = c
apply (Error e) _ = Error e
apply (Value x) (Value y) = parseError "Too many arguments."
apply (Function f) Empty  = parseError "Too few arguments."
apply (Function f) (Value x) = f x
apply (Function f) (Function g)  = Function $ \x -> apply (Function f) (g x)
apply (Value x) (Function f) = parseError "Missing operator."

compute :: Computation a -> Either String a
compute (Value x) = (Right x)
compute (Function f) = Left "Missing arguments."
compute (Error e) = Left e
compute Empty = Left "No computation required."

