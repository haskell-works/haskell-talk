{-# LANGUAGE FlexibleInstances #-}

module Lib where

import           Data.String
import           Prelude     hiding ((*), (**), (+), (-), (/))
import qualified Prelude     as P

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

f x = 3 * x ** 2 + 5 * x + 1
