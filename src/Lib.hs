{-# LANGUAGE FlexibleInstances #-}

module Lib where

import           Data.Semigroup hiding (Dual)
import           Data.String
import           Prelude        hiding ((*), (**), (+), (-), (/))
import qualified Prelude        as P

instance {-# OVERLAPPING #-} IsString [Bool] where
  fromString ('1':bs) = True :fromString bs
  fromString ('0':bs) = False:fromString bs
  fromString (_  :bs) =       fromString bs
  fromString []       = []

infixr 8  **
infixl 7  *, /
infixl 6  +, -

class Plus a where
  (+) :: a -> a -> a

class Minus a where
  (-) :: a -> a -> a

class Multiply a where
  (*) :: a -> a -> a

class Divide a where
  (/) :: a -> a -> a

class Power a where
  (**) :: a -> a -> a

instance Num Dual where
  fromInteger n = Dual (fromInteger n) 0
  (+) _ _   = error "Not implemented"
  (*) _ _   = error "Not implemented"
  abs _     = error "Not implemented"
  signum _  = error "Not implemented"
  negate _  = error "Not implemented"

instance Fractional Dual where
  fromRational n = Dual (fromRational n) 0
  (/) _ _ = error "Not implemented"

instance Plus Double where
  (+) u v = u P.+ v

instance Multiply Double where
  (*) u v = u P.* v

instance Minus Double where
  (-) u v = u P.- v

instance Divide Double where
  (/) u v = u P./ v

instance Power Double where
  (**) u v = u P.** v

instance Plus Dual where
  (+) (Dual u u') (Dual v v') = Dual (u + v) (u' + v')

instance Multiply Dual where
  (*) (Dual u u') (Dual v v') = Dual (u * v) (u' * v + u * v')

instance Minus Dual where
  (-) (Dual u u') (Dual v v') = Dual (u - v) (u' - v')

instance Divide Dual where
  (/) (Dual u u') (Dual v v') = Dual (u / v) ((u' * v - u * v') / v ** 2)

data Dual = Dual Double Double deriving (Eq, Show)

newtype AsciiRep = AsciiRep String deriving (Eq, Show)

instance Plus AsciiRep where
  (+) (AsciiRep u) (AsciiRep v) = AsciiRep (u <> " + " <> v)

instance Multiply AsciiRep where
  (*) (AsciiRep u) (AsciiRep v) = AsciiRep (u <> " * " <> v)

instance Minus AsciiRep where
  (-) (AsciiRep u) (AsciiRep v) = AsciiRep (u <> " - " <> v)

instance Divide AsciiRep where
  (/) (AsciiRep u) (AsciiRep v) = AsciiRep (u <> " / " <> v)

instance Power AsciiRep where
  (**) (AsciiRep u) (AsciiRep v) = AsciiRep (u <> " ** " <> v)

instance Num AsciiRep where
  fromInteger n = AsciiRep (show (fromInteger n))
  (+) _ _   = error "Not implemented"
  (*) _ _   = error "Not implemented"
  abs _     = error "Not implemented"
  signum _  = error "Not implemented"
  negate _  = error "Not implemented"

instance Fractional AsciiRep where
  fromRational n = AsciiRep (show (fromRational n))
  (/) _ _ = error "Not implemented"

data AsciiRep2 = AsciiRep2 String Int deriving (Eq, Show)

bracket :: Int -> Int -> String -> String
bracket p q u = if p <= q then u else "(" <> u <> ")"

instance Plus AsciiRep2 where
  (+) (AsciiRep2 u p) (AsciiRep2 v q) = AsciiRep2 (bracket 6 p u <> " + " <> bracket 6 q v) 6

instance Minus AsciiRep2 where
  (-) (AsciiRep2 u p) (AsciiRep2 v q) = AsciiRep2 (bracket 6 p u <> " - " <> bracket 6 q v) 6

instance Multiply AsciiRep2 where
  (*) (AsciiRep2 u p) (AsciiRep2 v q) = AsciiRep2 (bracket 7 p u <> " * " <> bracket 7 q v) 7

instance Divide AsciiRep2 where
  (/) (AsciiRep2 u p) (AsciiRep2 v q) = AsciiRep2 (bracket 7 p u <> " / " <> bracket 7 q v) 7

instance Power AsciiRep2 where
  (**) (AsciiRep2 u p) (AsciiRep2 v q) = AsciiRep2 (bracket 8 p u <> " ** " <> bracket 8 q v) 8

instance Num AsciiRep2 where
  fromInteger n = AsciiRep2 (show (fromInteger n)) 9
  (+) _ _   = error "Not implemented"
  (*) _ _   = error "Not implemented"
  abs _     = error "Not implemented"
  signum _  = error "Not implemented"
  negate _  = error "Not implemented"

instance Fractional AsciiRep2 where
  fromRational n = AsciiRep2 (show (fromRational n)) 9
  (/) _ _ = error "Not implemented"

asciiRep2 :: (AsciiRep2 -> AsciiRep2) -> String
asciiRep2 f = case f (AsciiRep2 "x" 9) of
  AsciiRep2 s _ -> s

f x = 3 * x ** 2 + 5 * x + 1

g x = x * (x + 1)
