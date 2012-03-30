module Main( main ) where

import System( getArgs )
import System.IO
import System.IO.Error
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as B
import Text.Printf
--import Text.Regex.Posix
import Text.Regex.PCRE.Light
import Control.Monad
import Dirs


textToBS :: T.Text -> B.ByteString
textToBS = B.concat . BL.toChunks . T.encodeUtf8

grep :: String -> Regex -> IO (Maybe Int)
grep fn s = 
  do withFile fn ReadMode $ ((return $!) =<<) . liftM search . T.hGetContents
  where
    search text =
      let r = filter ((\x -> isJust (match s (textToBS x) []) ). snd) $ zip [1..] . T.lines $ text in
      case r of [] -> Nothing
                (i,_):_ -> Just i

rgrep :: String -> String -> [String] -> IO (Int , Int)
rgrep pat dir kt = 
  do  acc <- foldSubDirs excl f (0,0) dir
      return acc
  where
    cpat= compile (B.fromString pat) []
    f Dir (f,d) fn = return (f,d+1)
    f File (f,d) fn = 
      do g <- grep fn cpat
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
        edefault = [ ".rake", ".ml", ".rb", ".hs", ".c", ".cpp", ".py", ".scala", ".java" ]
