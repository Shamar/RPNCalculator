-----------------------------------------------------------------------------
--
-- Module      :  Computation
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

module Computation (
    Computation,
    value,
    binary,
    unary,
    parseError,
    evaluate
) where

applyUntil :: (a -> a) -> (a -> Bool) -> a -> a
applyUntil f p x = if p x then x else applyUntil f p $ f x

isResult :: Expression a -> Bool
isResult (Result r) = True
isResult _ = False

evaluate :: (Show a) =>  (String -> Computation a) -> [String] -> Either String a

evaluate language tokens = compute $ current result
    where result = applyUntil (evaluateExpression language) isResult $ Expression Empty tokens

data Expression a =
      Result     {current :: Computation a}
    | Expression {current :: (Computation a),
                  rest :: [String]
                 }

instance (Show a) => Show (Expression a) where
    show (Result r) = "Result: " ++ show r
    show (Expression r rest) = "Expression (" ++ show r ++ ") " ++ show rest

data Computation a =
      Empty
    | Value a
    | Function (a -> Computation a)
    | Error String

value x = Value x
binary f = Function $ \x -> unary $ f x
unary  f = Function (\x -> Value $ f x)
parseError e = Error e


instance (Show a) => Show (Computation a) where
    show Empty = "Empty"
    show (Value x) = "Value " ++ show x
    show (Function f) = "Function"
    show (Error e) = "Error: " ++ e

evaluateExpression :: (Show a) => (String -> Computation a) -> Expression a -> Expression a
evaluateExpression _ (Expression result []) = Result result
evaluateExpression parse (Expression result otherTokens)
    | isResult exp = Result $ apply result $ current exp
    | otherwise    = evaluateExpression parse $ Expression (apply result $ current exp) $ rest exp
    where exp = evaluateExpression parse $ Expression Empty otherTokens

apply :: (Show a) => Computation a -> Computation a -> Computation a -- debugging
apply (Value x) (Value y) = parseError "Too many arguments."
apply Empty (Function f)  = parseError "Too few arguments."
apply (Function f) (Function g)  = parseError "Too many functions."
apply (Value x) (Function f) = f x
apply (Error e) _ = Error e

compute :: Computation a -> Either String a
compute (Value x) = (Right x)
compute (Function f) = Left "Missing one argument."
compute (Error e) = Left e
compute Empty = Left "No computation required."

evalLanguage :: (Show a) => (String -> Computation a) -> Computation a -> String -> Computation a
evalLanguage language currentComputation token = apply (language token) currentComputation

