{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (void)
import Control.Monad.State
import Data.List (intercalate)
import Data.Ratio
import qualified Data.Vector as V
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

data AbacusState = AbacusState
  { steps :: [String]
  , matrix :: V.Vector (V.Vector Rational)
  }
type Abacus = StateT AbacusState IO
updateAbacus :: Maybe (V.Vector (V.Vector Rational)) -> String -> Abacus ()
updateAbacus res step = case res of
  Nothing -> liftIO $ putStrLn "[ERROR] Index out of bounds!"
  Just nextMatrix -> modify \st -> st 
    { steps = step : steps st
    , matrix = nextMatrix 
    }

swapRows :: Int -> Int -> V.Vector a -> Maybe (V.Vector a)
swapRows aIdx bIdx m = do
  r_a <- m V.!? aIdx
  r_b <- m V.!? bIdx
  pure $ m V.// [(bIdx, r_a), (aIdx, r_b)]
multiplyRow :: Rational -> Int -> V.Vector (V.Vector Rational) -> Maybe (V.Vector (V.Vector Rational))
multiplyRow c aIdx m = do
  r_a <- m V.!? aIdx
  pure $ m V.// [(aIdx, V.map (* c) r_a)]
rowReplacement :: Rational -> Int -> Int -> V.Vector (V.Vector Rational) -> Maybe (V.Vector (V.Vector Rational))
rowReplacement c aIdx bIdx m = do
  r_a <- m V.!? aIdx
  r_b <- m V.!? bIdx
  pure $ m V.// [(bIdx, V.zipWith (\r_ax r_bx -> c * r_ax + r_bx) r_a r_b)]

class ScalarInput a where
  parseScalar :: String -> Maybe a
instance ScalarInput Int where
  parseScalar = readMaybe
instance ScalarInput Rational where
  parseScalar line = 
    case readMaybe (map (\c -> if c == '/' then '%' else c) line) of
      Just r  -> Just r
      Nothing -> toRational <$> (readMaybe line :: Maybe Double)

readInput :: forall a. ScalarInput a => String -> IO a
readInput prompt = do
  putStr prompt
  hFlush stdout
  line <- getLine
  case parseScalar line of
    Just n  -> pure n
    Nothing -> do
      putStrLn "[ERROR] Invalid input format!"
      readInput prompt

showRational :: Rational -> String
showRational r
  | denominator r == 1 = show $ numerator r
  | otherwise          = show (numerator r) ++ "/" ++ show (denominator r)
showMatrix :: V.Vector (V.Vector Rational) -> String
showMatrix = intercalate "\n" . V.toList . V.map (intercalate "\t" . V.toList . V.map showRational)

bead :: Abacus ()
bead = do
  AbacusState s m <- get

  liftIO do
    putStrLn ""
    putStrLn $ showMatrix m
    putStrLn $ "Steps History: " ++ if null s then "None" else intercalate " => " (reverse s)
    putStrLn "Please enter your operation:"
    putStrLn "  (1) Swap Rows"
    putStrLn "  (2) Multiply Row"
    putStrLn "  (3) Row Replacement"

  opt <- liftIO $ readInput "Operation [1, 3]: " :: Abacus Int
  
  case opt of
    1 -> do
      liftIO $ putStrLn "[SELECTED] (1) Swap Rows: `R_{A} ↔ R_{B}`"

      a <- liftIO $ readInput ("Row A [1, " ++ show (V.length m) ++ "]: ") :: Abacus Int
      b <- liftIO $ readInput ("Row B [1, " ++ show (V.length m) ++ "]: ") :: Abacus Int
      
      let aIdx = a - 1
      let bIdx = b - 1

      updateAbacus (swapRows aIdx bIdx m) ("`R_{" ++ show a ++ "} ↔ R_{" ++ show b ++ "}`")
    2 -> do
      liftIO $ putStrLn "[SELECTED] (2) Multiply Row: `C * R_{A} → R_{A}`"

      c <- liftIO $ readInput "Constant C: " :: Abacus Rational
      a <- liftIO $ readInput ("Row A [1, " ++ show (V.length m) ++ "]: ") :: Abacus Int

      let aIdx = a - 1

      updateAbacus (multiplyRow c aIdx m) ("`" ++ showRational c ++ " * R_{" ++ show a ++ "} → R_{" ++ show a ++ "}`")
    3 -> do
      liftIO $ putStrLn "[SELECTED] (3) Row Replacement: `C * R_{A} + R_{B} → R_{B}`"

      c <- liftIO $ readInput "Constant C: " :: Abacus Rational
      a <- liftIO $ readInput ("Row A [1, " ++ show (V.length m) ++ "]: ") :: Abacus Int
      b <- liftIO $ readInput ("Row B [1, " ++ show (V.length m) ++ "]: ") :: Abacus Int

      let aIdx = a - 1
      let bIdx = b - 1

      updateAbacus (rowReplacement c aIdx bIdx m) ("`" ++ showRational c ++ " * R_{" ++ show a ++ "} + R_{" ++ show b ++ "} → R_{" ++ show b ++ "}`")
    _ -> liftIO $ putStrLn "[ERROR] Invalid option!"
  bead

readMatrix :: IO (V.Vector (V.Vector Rational))
readMatrix = do
  m <- readInput "Matrix's m: " :: IO Int
  n <- readInput "Matrix's n: " :: IO Int
  V.generateM m \r ->
    V.generateM n \c ->
      readInput $ "Matrix Element [" ++ show (r + 1) ++ ", " ++ show (c + 1) ++ "]: "

main :: IO ()
main = do
  initMatrix <- readMatrix
  void $ runStateT bead (AbacusState [] initMatrix)