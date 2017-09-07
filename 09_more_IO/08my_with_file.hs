import System.IO
import Control.Exception

myWithFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
myWithFile name mode f = bracket (openFile name mode) (\handle -> hClose handle) (\handle -> f handle)

main = do
    withFile "baabaa.txt" ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStr contents
