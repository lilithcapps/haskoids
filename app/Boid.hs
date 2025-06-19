{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Boid where
import           Data.Bifunctor (Bifunctor (bimap))

type PositionVector = Vector
type MovementVector = Vector
type Vector = (Float, Float)

data Boid = Boid {
  position :: PositionVector,
  velocity :: MovementVector
  }
  deriving Show

x :: (a, b) -> a
x = fst
y :: (a, b) -> b
y = snd

instance Num Vector where
  (+) a b = (x a + x b, y a + y b)
  (*) a b = (x a * x b, y a * y b)
  abs = bimap abs abs
  signum = bimap signum signum
  fromInteger i = (fromInteger i :: Float, fromInteger i :: Float)
  negate = bimap negate negate

mult :: Float -> Vector -> Vector
mult a v = (fst v * a, snd v * a)

magnitude :: Vector -> Float
magnitude v = sqrt $ sqr (x v) + sqr (y v)
  where
    sqr a = a * a

unit :: Vector -> Vector
unit v = (1 / magnitude v) `mult` v

dist :: Boid -> Boid -> Float
dist b1 b2 =
  let pos1 = position b1 in
  let pos2 = position b2 in
  let dX = x pos1 - x pos2 in
  let dY = y pos1 - y pos2 in
  dX ** 2 + dY ** 2

