
module Operator where
import DbStream
import Tuple
import Abstract



filterOp :: (Tuple -> Either EvalError Bool) -> DbStream Tuple Tuple Int
filterOp pred = Read f where
    f Nothing = Return 0
    f (Just t) = 
        case pred t of
            Right True -> do
                res <- Write t (filterOp pred)
                Return $ res + 1
            Right False ->
                filterOp pred
            Left err->
                Return (-1)

