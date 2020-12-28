-- |
-- Module      :  String.Languages.UniquenessPeriods.VectorG
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Generalization of the uniqueness-periods and uniqueness-periods-general 
-- packages functionality.
-- 

{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses #-}

module String.Languages.UniquenessPeriods.VectorG where

import GHC.Int
import qualified Data.Vector as VB

data UniquenessGeneral1 a b = UG1 a [b] (VB.Vector b) | UG2 a [b] (VB.Vector b) | UG3 [b] (VB.Vector b) deriving Eq

class UniquenessGeneral a b where
  get :: a -> b

type UniquenessGeneral2 a = VB.Vector ([Int16], a)  
  
instance (Eq a) => UniquenessGeneral (UniquenessGeneral1 Bool a) (UniquenessGeneral2 a) where
  get (UG1 y whspss v) = uniquenessPeriodsVector1 y whspss v
  get (UG2 y whspss v) = uniquenessPeriodsVector2 y whspss v 
  get (UG3 whspss v) = uniquenessPeriodsVector3 whspss v 

-- | List of 'Int16' in the result is a list of indexes for the occurrences of the value of the @a@ (usually, @a@ is a sound representation or its duration). 
-- The first 'Bool' argument defines whether to apply the filtering for not informative (possibly) \"whitespace symbols\" given as the 
-- second argument list. The resulting 'VB.Vector' is sorted in the order of the first occurrence of each of the @a@ (usually, @a@ is the sound 
-- representation, or its duration, or some other its characteristics) in the given third argument.
uniquenessPeriodsVector1 :: Eq a => Bool -> [a] -> VB.Vector a -> UniquenessGeneral2 a
uniquenessPeriodsVector1 y whspss v 
 | VB.null v = VB.empty
 | otherwise = let !v1 = VB.indexed v in 
    let f !x = if VB.null x then Nothing 
                else Just . (\(v2,v3) -> ((VB.toList . VB.map fst $ v2,snd . VB.unsafeIndex v2 $ 0),v3)) . 
                  VB.partition (\(_,xs) -> xs == (snd . VB.unsafeIndex x $ 0)) $ x in 
                   (if y then VB.map (\(js,t) -> (map toEnum js,t)) . VB.filter (\(_,!zs) -> zs `notElem` whspss) else VB.map (\(js,t) -> (map toEnum js,t))) . VB.unfoldr f $ v1 

-- | List of 'Int16' in the result is a list of distances between the consequential occurrences of the @a@ (usually, @a@ is a sound representation or its duration)
-- in the given 'VB.Vector'. The first 'Bool' argument defines whether to apply the filtering for not informative
-- (possibly) \"whitespace symbols\" given as the second argument list. The resulting 'VB.Vector' is sorted in the order of the first occurrence of each of 
-- the @a@ (usually, @a@ is the sound representation or its duration, or some other its characteristics) in the given third argument.
uniquenessPeriodsVector2 :: Eq a => Bool -> [a] -> VB.Vector a -> UniquenessGeneral2 a
uniquenessPeriodsVector2 y whspss v 
 | VB.null v = VB.empty
 | otherwise = let !v1 = VB.indexed v in 
    let f !x = if VB.null x then Nothing 
                else Just . (\(v2,v3) -> ((VB.toList . (\v4 -> VB.zipWith subtract v4 (VB.unsafeSlice 1 (VB.length v4 -1) v4)) . VB.map fst $ v2,snd . 
                  VB.unsafeIndex v2 $ 0),v3)) . VB.partition (\(_,xs) -> xs == (snd . VB.unsafeIndex x $ 0)) $ x in 
                    (if y then VB.map (\(js,t) -> (map toEnum js,t)) . VB.filter (\(ys,!zs) -> not (null ys) && zs `notElem` whspss) else VB.map (\(js,t) -> (map toEnum js,t))) . VB.unfoldr f $ v1

-- | List of 'Int16' in the result is a list of distances between the consequential occurrences of the @a@ (usually, @a@ is a sound representation or its duration)
-- in the given 'VB.Vector'. But unlikely the 'uniquenessPeriodsVector2' function it finds out only the distances for the repeated not \"whitespece symbols\" 
-- occurring in different sublists separated with these \"whitespace symbols\". Therefore, it is much more perceptable for the words order than the former one. 
-- The resulting 'VB.Vector' is sorted in the order of the first occurrence of each of 
-- the @a@ (usually, @a@ is the sound representation or its duration, or some other its characteristics) in the given second argument.
uniquenessPeriodsVector3 :: Eq a => [a] -> VB.Vector a -> UniquenessGeneral2 a
uniquenessPeriodsVector3 whspss v 
 | VB.null v = VB.empty
 | otherwise = let !v1 = VB.indexed v in let !vs = VB.toList . VB.map toEnum . VB.findIndices (`elem` whspss) $ v in 
    let f !x = if VB.null x then Nothing 
                else let !idX0 = snd . VB.unsafeIndex x $ 0 in Just . (\vws (v2,v3) -> ((helpUPV3 vws [] . VB.toList . VB.map fst $ v2,snd . 
                  VB.unsafeIndex v2 $ 0),v3)) vs . VB.partition (\(_,xs) -> xs == idX0) $ x in 
                    VB.filter (\(ys,!zs) -> not (null ys) && (zs `notElem` whspss)) . VB.unfoldr f . VB.map (\(j,t) -> (toEnum j,t)) $ v1

-- | Is used inside the 'uniquenessPeriodsVector3'. The first and the third list arguments of non-negative numbers (if not empty) must be sorted in the ascending order.
helpUPV3 :: [Int16] -> [Int16] -> [Int16] -> [Int16]
helpUPV3 (z:zs) !acc (x:y:xs) 
 | compare ((x - z) * (y - z)) 0 == LT = helpUPV3 zs ((y - x):acc) (y:xs)
 | compare y z == GT = helpUPV3 zs acc (x:y:xs)
 | otherwise = helpUPV3 (z:zs) acc (y:xs)
helpUPV3 _ !acc _ = acc

