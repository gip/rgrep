module Main( main ) where

import System( getArgs )
import System.IO
import System.IO.Error
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
--import qualified Data.Text as T
--import qualified Data.Text.IO as T
import Data.List
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

rgrep pat dir = 
  do  args <- getArgs
      acc <- foldSubDirs excl f (0,0) dir
      return acc
  where
    f Dir (f,d) fn = return (f,d+1)
    f File (f,d) fn = 
      do g <- grep fn pat
         case g of Just i -> putStr $ printf "Match at line %2d: %s\n" i fn
                   _ -> return ()
         ff <- return $ case g of Just _ -> f+1
                                  _ -> f
         return (ff,d)
    excl n = not $ ( foldr (\k a -> (isSuffixOf k n) || a) False kt )
      where
        kt = [ ".rake", ".ml", ".rb", ".hs", ".c", ".cpp", ".py" ]

main =
  do (p,d) <- liftM parse $ getArgs
     if p=="" then putStrLn "rgrep: usage: rgrep <pattern> [<directory>]"
              else do rgrep p d
                      return ()
     return ()
  where
    parse a = case a of p:d:_ -> (p,d)
                        [p] -> (p,".")
                        _ -> ("","")
