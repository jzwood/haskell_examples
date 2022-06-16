module EitherMonads where

import Data.List
import Data.Functor
import Data.Function


johns = ["john", "sally", "rob", "john", "john", "john"]

-- GOOD

countJohns :: [String] -> Either String Integer
countJohns names
  | numJohns < 1 = Left "NO JOHNS!"
  | otherwise = Right numJohns
  where
    numJohns = genericLength (filter (=="john") names)

exaggerateCount :: Integer -> Integer
exaggerateCount x = x + 10

purityCheck :: Integer -> Either String Integer
purityCheck numJohns
  | numJohns < 12 = Left "there are nowhere near enough Johns"
  | otherwise = Right numJohns

purityCheck' :: Integer -> Either String Integer
purityCheck' numJohns
  | odd numJohns = Left "odd Johns are not allowed"
  | otherwise = Right numJohns

praiseOrShun :: Either String Integer -> String
praiseOrShun (Left msg) = "You are unworthy because " ++ msg
praiseOrShun (Right count) = "Praise all " ++ show count ++ " JOHNS"

johnPipeline names = countJohns names
                   <&> exaggerateCount  -- fmap with arguments flipped
                   >>= purityCheck  -- monadic bind
                   >>= purityCheck'
                   & praiseOrShun  -- & is the same as |>









-- BAD

exaggerateCountAdapter :: Either String Integer -> Either String Integer
exaggerateCountAdapter (Right i) = Right (exaggerateCount i)
exaggerateCountAdapter bad = bad

purityCheckAdapter :: Either String Integer -> Either String Integer
purityCheckAdapter (Right numJohns) = purityCheck numJohns
purityCheckAdapter bad = bad

purityCheckAdapter' :: Either String Integer -> Either String Integer
purityCheckAdapter' (Right numJohns) = purityCheck' numJohns
purityCheckAdapter' bad = bad

johnPipeline2 names = countJohns names
                    & exaggerateCountAdapter  -- & is the same as |>
                    & purityCheckAdapter
                    & purityCheckAdapter'
                    & praiseOrShun

main :: IO ()
main =  do
  print $ johnPipeline johns
  print $ johnPipeline2 johns
