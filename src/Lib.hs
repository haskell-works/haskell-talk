{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Lib where

import Data.String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

instance {-# OVERLAPPING #-} IsString [Bool] where
  fromString ('1':bs) = True :fromString bs
  fromString ('0':bs) = False:fromString bs
  fromString (_  :bs) =       fromString bs
  fromString []       = []
