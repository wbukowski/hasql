
module DbStream where 

import Tuple ( Mergable(..) )

import Control.Monad ( ap, (>=>) )
import System.IO (isEOF)

data DbStream i o a
  = Return a
  | Read (Maybe i -> DbStream i o a)
  | Write o (DbStream i o a) 

instance Functor (DbStream i o ) where
    fmap f m = m >>= return . f

instance Applicative (DbStream i o) where
    pure = return
    (<*>) = ap

instance Monad (DbStream i o) where
    return = Return
    (>>=) (Return a) f = 
        f a

    (>>=) (Read func) f = 
        Read $ func >=> f

    (>>=) (Write o m) f = 
        Write o (m >>= f)

read = Read
write = Write

(|>|) :: DbStream i m a -> DbStream m o b -> DbStream i o b
(|>|) (Write o s1) (Read f) = 
    s1 |>| f (Just o)
(|>|) s1 (Return b) = 
    Return b
(|>|) s1 (Write o s2) = 
    Write o $ s1 |>| s2
(|>|) (Read f) s2 = 
    Read (\c -> f c |>| s2)
(|>|) s1 (Read f) =
    s1 |>| f Nothing 

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

combine :: Mergable o => DbStream i o a -> DbStream i o b -> DbStream i o a

combine (Return x) s2 = 
    Return x
combine (Read f) s2 = do
    Read (\r -> combine (f r) s2)
combine (Write o s1) s2 = do
    (r2, l2) <- catchOutput s2
    writeMergeList (cartProd [o] l2) (combine s1 s2)
    where 
        writeMergeList :: Mergable o => [(o, o)] -> DbStream i o a -> DbStream i o a
        writeMergeList [] end = end
        writeMergeList ((x1, x2) : xs) end = Write (merge x1 x2) (writeMergeList xs end)

catchOutput :: DbStream i o a -> DbStream i b (a, [o])
catchOutput (Return a) =
  Return (a, [])
catchOutput (Read f) =
  Read (catchOutput . f)
catchOutput (Write o s) =
  s' >>= (\(a, xs) -> Return (a, o : xs))
  where
    s' = catchOutput s

listTrans :: DbStream i o a -> [i] -> ([o], a)
listTrans (Return x) _ =
    ([], x)
listTrans (Read f) xs =
    listTrans (f hd) tl 
    where
        hd = case xs of 
            [] -> Nothing
            (h : _) -> Just h
        tl = case xs of 
            [] -> []
            (_ : t) -> t
listTrans (Write o s) xs = 
    let (ys, res) = listTrans s xs
    in (o : ys, res)

fromList :: [o] -> a -> DbStream i o a
fromList xs res =
    foldr Write (Return res) xs

streamMap :: (o -> p) -> DbStream i o a -> DbStream i p a
streamMap f (Return x) = 
    Return x
streamMap f (Read con) = 
    Read (streamMap f . con)
streamMap f (Write o s) = 
    Write (f o) (streamMap f s)