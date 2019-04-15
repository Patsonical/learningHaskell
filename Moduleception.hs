module Moduleception where

inception :: Int -> String -> String
inception n s = if n > 0 then inception (n-1) ("["++s++"]") else s
