module Main where

import LineVerification
import Graphics.DVI as DVI
import TestText
import Control.Monad.State
import Data.Maybe
import Data.List
import System.Directory
import Linebreaking as LB
import Utilities
import Examples
import System.IO

main :: IO ()
main = do

  ---generates the examples and places them in the /file directory
  twoParagraphs
  patching
  marginPunctuation
  authorLines
  raggedRight
  centeredText
  kerning
  multiGlue
  explicitBreaks
  emptyLine
  flaggedPenalty
  prohibitedBreak
  complexIndex
  differentLineWidths
  
  let 
    weightList1 = [0,3,3,2,2,2,1,1,1,0,0,0]
    weightList2 = [0,0,0,1,1,1,2,2,3,3,2,2]
    wordAmount1 = 50
    wordAmount2 = 200
    testAmount = 1
  
  --runs tests and writes the results to the /files directory. note that higher testAmounts can take a long time
  runMultiples "testfileShort50.txt" testAmount weightList1 wordAmount1
  runMultiples "testfileShort200.txt" testAmount weightList1 wordAmount2
  runMultiples "testfileLong50.txt" testAmount weightList2 wordAmount1
  runMultiples "testfileLong200.txt" testAmount weightList2 wordAmount2
  
  return ()

