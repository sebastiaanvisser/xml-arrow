{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.List
import Control.Category
import Prelude hiding (elem, (.), id)
import Text.XML.Light
import Text.XML.Light.Arrow
import Safe

main :: IO ()
main =
  do testVal (plantNames . plants)
     testXml (plantPrices . plants)
     testVal (plantNames . expensivePlants)
     testXml (decreasePrices . expensivePlants)

  -- Utility functions:
  where testVal ar = example >>= mapM_ print                 . runListArrow (ar . unlistA)
        testXml ar = example >>= mapM_ (putStrLn . printXml) . runListArrow (ar . unlistA)
        example    = readFile "plant-catalog.xml" >>= return . parseXML

-- Test arrows:

plants :: ArrowList (~>) => Content ~> Content
plants = child "plant" . elem "catalog"

plantNames :: (ArrowPlus (~>), ArrowList (~>)) => Content ~> (String, String)
plantNames = common &&& botanical
  where common    = text . children . child "common"
        botanical = text . children . child "botanical"

plantPrices :: ArrowList (~>) => Content ~> Content
plantPrices = child "price"

expensivePlants :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => Content ~> Content
expensivePlants = filterA expensive . plants
  where prices    = deepText . child "price"
        expensive = isA (> (9.5 :: Double)) . maybeL . arr parse . prices
        parse     = readMay . tail

decreasePrices :: (ArrowList (~>), ArrowApply (~>), ArrowChoice (~>)) => Content ~> Content
decreasePrices = processDeep (elem "price") (process1 (processText inc))
  where inc = arr (show . (+ (-2 :: Double)) . read . tail)

