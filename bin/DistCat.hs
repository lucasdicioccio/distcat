
module Main where

import Tool.DistCat
import System.Environment (getArgs)
import System.IO (hSetBuffering,stdout,stdin)
import System.IO (BufferMode (LineBuffering))

pickDist :: [String] -> Maybe (String -> String)
pickDist ["edit-distance"]      = Just $ printDist editDistance defaultFormat
pickDist ["substr-similarity"]  = Just $ printDist longestSubstringSimilarity defaultFormat
pickDist _                      = Nothing

usage :: String
usage = "distcat <edit-distance|substr-similarity>"

main :: IO ()
main = do 
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering
    d <- getArgs >>= (return . pickDist)
    case d of
        Just f  -> interact f
        Nothing -> print usage
          
