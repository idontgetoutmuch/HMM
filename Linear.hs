{-# OPTIONS_GHC -Wall            #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedLists     #-}

import qualified Naperian as N
import           GHC.TypeLits
import           Data.List
import qualified Data.Vector as V
import           Control.Applicative (liftA2)


matToFun0 :: forall m n a . (KnownNat m, KnownNat n) =>
             N.Hyper '[N.Vector m, N.Vector n] a ->
             N.Finite n -> N.Vector m a
matToFun0 x = N.lookup (N.point $ N.crystal $ N.crystal x)

matToFun :: forall m n a . (KnownNat m, KnownNat n) =>
            N.Hyper '[N.Vector m, N.Vector n] a ->
            N.Finite n -> (N.Finite m -> a)
matToFun x = N.lookup . matToFun0 x

initC :: forall m n a . (KnownNat m, KnownNat n, Num a, Floating a) =>
         N.Hyper '[N.Vector m] a ->
         N.Hyper '[N.Vector m, N.Vector n] a ->
         N.Finite n ->
         (N.Finite m -> a)
initC initP obsM x = N.lookup $ fmap log $
                     N.zipWith (*) initQ (matToFun0 obsM x)
  where
    initQ  = N.point $ N.crystal initP

updC :: forall m n a . (KnownNat m, KnownNat n, Num a, Floating a, Ord a) =>
          N.Hyper '[N.Vector m, N.Vector m] a ->
          N.Hyper '[N.Vector m, N.Vector n] a ->
          (N.Finite m -> a) ->
          N.Finite n ->
          (N.Finite m -> a)
updC updM obsM f1 x = liftA2 (+) f4 (log . matToFun obsM x)
  where
    f4 zCurr = let N.Vector u = N.tabulate f5 in V.maximum u
      where
        f5 zPrev = f1 zPrev + log (matToFun updM zPrev zCurr)

forwards :: forall t m n a . (Traversable t, KnownNat m, KnownNat n, Floating a, Ord a) =>
            N.Hyper '[N.Vector m] a ->
            N.Hyper '[N.Vector m, N.Vector m] a ->
            N.Hyper '[N.Vector m, N.Vector n] a ->
            N.Finite n ->
            t (N.Finite n) ->
            (N.Finite m -> a, t (N.Finite m -> a))
forwards initP updM obsM x xs =
  mapAccumL (\s y -> let u = updC updM obsM s y in (u, u))
            (initC initP obsM x) xs

forwardsCo :: (Floating a, KnownNat m, KnownNat n, Ord a) =>
        N.Hyper '[N.Vector m] a
     -> N.Hyper '[N.Vector m, N.Vector m] a
     -> N.Hyper '[N.Vector m, N.Vector n] a
     -> [N.Finite n]
     -> [N.Finite m -> a]
forwardsCo initP updM obsM xs = hs
  where
    hs = (initC initP obsM (head xs)) : (zipWith ff hs (tail xs))
    ff = updC updM obsM

eps, tau :: Double
eps = 0.05
tau = 0.05

transitionMatrix :: N.Hyper '[N.Vector 2, N.Vector 2] Double
transitionMatrix = N.Prism $ N.Prism $ N.Scalar [ [1 - eps, eps]
                                                , [eps, 1 - eps]
                                                ]

emissionProbs :: N.Hyper '[N.Vector 2, N.Vector 2] Double
emissionProbs = N.Prism $ N.Prism $ N.Scalar [ [1 - tau, tau]
                                             , [tau, 1 - tau]
                                             ]

initialDistribution :: N.Hyper '[N.Vector 2] Double
initialDistribution = N.Prism $ N.Scalar [1.0, 0.0]

d :: V.Vector (N.Finite 2)
d = V.map N.Fin $
    [0,0,0,0,0,1,1,1,1,1] <>
    [0,0,0,0,0,0,0,0,0,0] <>
    [0]

fs :: V.Vector (N.Finite 2 -> Double)
fs = snd $ forwards initialDistribution transitionMatrix emissionProbs (V.head d) (V.tail d)

fCos :: [N.Finite 2 -> Double]
fCos = forwardsCo initialDistribution transitionMatrix emissionProbs (V.toList d)

test :: [Double]
test = map ($(N.Fin 0)) fCos

test1 :: V.Vector Double
test1 = V.map ($(N.Fin 0)) fs
