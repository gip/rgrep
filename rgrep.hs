module Main( main ) where

import System( getArgs )
import System.IO
import System.IO.Error
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
--import qualified Data.Text as T
--import qualified Data.Text.IO as T
import Data.List
import Data.Maybe
import Text.Printf
import Control.Monad
import Dirs

grep :: String -> String -> IO (Maybe Int)
grep fn s = 
  do withFile fn ReadMode $ ((return $!) =<<) . liftM search . T.hGetContents
  where
    search text =
      let ps= T.pack s in
      let r = filter (T.isInfixOf ps . snd) $ zip [1..] . T.lines $ text in
      case r of [] -> Nothing
                (i,_):_ -> Just i

rgrep pat dir kt = 
  do  args <- getArgs
      acc <- foldSubDirs excl f (0,0) dir
      return acc
  where
    f Dir (f,d) fn = return (f,d+1)
    f File (f,d) fn = 
      do g <- grep fn pat
         when (isJust g)  $ printf "Match at line %2d: %s\n" (fromJust g) fn
         return (if isJust g then f+1 else f,d)
    excl n = not $ ( foldr (\k a -> (isSuffixOf k n) || a) False kt )

main =
  do (p,d,exc) <- liftM parse $ getArgs
     if p=="" then putStrLn "rgrep: usage: rgrep <search pattern> [<directory>] [<file types>]"
              else do rgrep p d exc
                      return ()
     return ()
  where
    parse a = case a of p:d:es -> (p,d,if null es then edefault else map (\n -> '.':n) es)
                        [p] -> (p,".",edefault)
                        _ -> ("","",[])
      where
        edefault = [ ".rake", ".ml", ".rb", ".hs", ".c", ".cpp", ".py" ]
