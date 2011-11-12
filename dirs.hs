module Dirs( foldSubDirs, Objt(Dir,File) ) where

import System.Directory
import Control.Monad

data Objt = Dir | File
 
foldSubDirs :: (String -> Bool) -> (Objt -> a -> String -> IO a) -> a -> String -> IO a
foldSubDirs excl f a0 path =
  do dc <- getDirectoryContents path
     foldM rec a0 dc
  where
    rec a ('.':_) = return a
    rec a dir = 
      let fn = path++"/"++dir in
      do d <- doesDirectoryExist fn
         if d then do --putStrLn fn 
                      ad <- foldSubDirs excl f a fn
                      res <- f Dir ad fn
                      return res
              else do res <- if excl fn then return a else f File a fn
                      return res
 