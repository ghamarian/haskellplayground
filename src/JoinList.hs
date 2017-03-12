{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Sized
import Data.Monoid
import Test.QuickCheck

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (JoinList Size a) where
   arbitrary = sized arbJoin

arbJoin ::  Arbitrary a => Int -> Gen (JoinList Size a)
arbJoin 0  = Single <$> return (Size 1) <*> arbitrary
arbJoin n = frequency [(1, singleGen), (3, appendGen)] where
               singleGen = do
                  a <- arbitrary
                  return $ Single (Size 1) a
               appendGen = do
                  a <- shrub
                  b <- shrub
                  return (a +++ b)
               shrub = arbJoin (n `div` 2)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ Empty = a
a +++ b = Append (tag a <> tag b) a b

whatSize :: (Sized b, Monoid b) => JoinList b a -> Int
whatSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n Empty = Nothing
indexJ n (Single m a)  |  n == 0 = Just a
                       |  otherwise = Nothing
indexJ n (Append m s1 s2) | n >= whatSize s1 = indexJ (n - whatSize s1) s2
                          | otherwise = indexJ n s1 

{-dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a-}
{-dropJ 0 x = x-}
{-dropJ n m | n > whatSize m = Empty-}
{-dropJ n (Single m s) | n >= 1 = Empty-}
{-dropJ n (Append m x y) | n > whatSize x = dropJ (n - whatSize x) y-}
                       {-| otherwise = dropJ n x +++ y-}

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 x = x
dropJ n m | n >= whatSize m = Empty
{-dropJ n (Single _ _) | n >= 1   = Empty-}
dropJ n (Append _ l1 l2) | n < whatSize l1 = dropJ n l1 +++ l2
                         | otherwise = dropJ (n - whatSize l1) l2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n Empty = Empty
takeJ 0 n = Empty
takeJ n a | n >= whatSize a = a
takeJ n (Append m x y) | n > whatSize x = x +++ takeJ (n - whatSize x) y
                         | otherwise = takeJ n x

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

fromString' :: [String] -> JoinList Size String
fromString' = foldr (\x j -> Single (Size 1) x +++ j) Empty

prop_index :: Eq a => NonNegative Int -> JoinList Size a -> Bool
prop_index (NonNegative i) jl = indexJ i jl == (jlToList jl !!? i)

prop_drop :: (Eq a) => NonNegative Int -> JoinList Size a -> Bool
prop_drop (NonNegative n) jl = jlToList (dropJ n jl) == drop n (jlToList jl)

prop_take :: Eq a => NonNegative Int -> JoinList Size a -> Bool
prop_take (NonNegative n) jl = jlToList (takeJ n jl) == take n (jlToList jl)
