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

isResult :: Expression a -> Bool
isResult (Result r) = True
isResult _ = False


evaluate :: (Show a) =>  (String -> Computation a) -> [String] -> Either String a
evaluate language tokens = compute $ current result
    where result = until isResult (evaluateExpression language) $ Expression Empty tokens

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
binary f = Function $ \x -> unary $ (flip f) x
unary  f = Function (\x -> Value $ f x)
parseError = Error

instance (Show a) => Show (Computation a) where
    show Empty = "Empty"
    show (Value x) = "Value " ++ show x
    show (Function f) = "Function"
    show (Error e) = "Error: " ++ e

evaluateExpression :: (Show a) => (String -> Computation a) -> Expression a -> Expression a
evaluateExpression _ (Expression result []) = Result result
evaluateExpression parse (Expression (Function f) rest) = Expression (Function f) rest
evaluateExpression parse (Expression result (x:xs))
    | isResult nextExp = Result $ apply result $ current nextExp
    | otherwise = evaluateExpression parse $ Expression (apply result $ current nextExp) $ rest nextExp
    where nextExp = evaluateExpression parse $ Expression (parse x) xs

apply :: (Show a) => Computation a -> Computation a -> Computation a
apply Empty (Value x) = Value x
apply Empty (Function f)  = parseError "Too few arguments."
apply Empty (Error e)  = (Error e)
apply (Value x) (Value y) = parseError "Too many arguments."
apply (Value x) (Function f) = f x
apply (Function f) (Function g)  = parseError "Too many functions."
apply (Error e) _ = Error e
apply x y = parseError $ "Can not apply " ++ show x ++ " to " ++ show y ++ "."

compute :: Computation a -> Either String a
compute (Value x) = (Right x)
compute (Function f) = Left "Missing one argument."
compute (Error e) = Left e
compute Empty = Left "No computation required."

