import System.Environment
import System.IO  
import Control.Monad
import Data.List
import Data.List.Split
import ClosestPair


pairify :: [Int] -> (Int, Int)
pairify [x,y] = (x,y)


convert_int :: [String] -> [Int]
convert_int = map read

getLines = liftM lines . readFile


main = do  
        (filepath:args) <- getArgs
        list <- getLines filepath
        let list' = [ pairify $ convert_int (splitOn "," xs)  | xs <- list] in
           print $ closest_pair list'
