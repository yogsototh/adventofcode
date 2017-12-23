{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
|-}

module DayXX where

import           Protolude

-- import           Control.Lens          hiding ((&))
-- import           Data.Array
-- import           Data.Generics.Product
-- import qualified Data.Text             as T
import           GHC.Generics

type Input = [Text]

parseInput :: IO Input
parseInput = parseText <$> readFile "inputs/day19.txt"

testInput :: Text
testInput = ""

parseText :: Text -> Input
parseText = undefined

-- Solution 1

type Solution1 = Int

data AppState = AppState () deriving (Show,Eq,Generic)

solution1 :: Input -> Solution1
solution1 grid =
  let initState = AppState ()
  in extractSol $ execState solve1 initState
  where
    extractSol :: AppState -> Solution1
    extractSol = undefined
    solve1 :: State AppState ()
    solve1 = undefined

-- Solution 2

type Solution2 = Int

solution2 :: Input -> Solution2
solution2 grid =
  let initState = AppState ()
  in extractSol $ execState solve2 initState
  where
    extractSol :: AppState -> Solution2
    extractSol = undefined
    solve2 :: State AppState ()
    solve2 = undefined
