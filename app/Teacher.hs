{-# LANGUAGE ImportQualifiedPost #-}

import System.IO (hFlush, hPutStrLn, stderr, stdout)

import Data.IORef
import Data.Maybe (isJust)
import System.Exit (exitFailure)
import System.IO

import ExampleAutomata
import IO
import Nominal (Atom)
import OrbitList qualified
import Support (Rat (..))

data Example
  = Fifo Int
  | DoubleWord Int

main :: IO ()
main =
  let ex = Fifo 2
   in case ex of
        Fifo n -> teach "FIFO" (fifoFun n) (fifoCex n)
        DoubleWord n -> teach "ATOMS" (doubleFun n) (doubleCex n)

teach :: (ToStr a, FromStr a, Show a) => String -> ([a] -> Bool) -> [[a]] -> IO ()
teach alphStr fun cexes = do
  -- Set up some counters
  countMQ <- newIORef (0 :: Int)
  countEQ <- newIORef (0 :: Int)
  cexChecks <- newIORef cexes

  let
    -- Helper functions for answering/logging
    log str = hPutStrLn stderr str
    answer str = hPutStrLn stdout str >> hFlush stdout

    -- Parsing the commands from the learner
    act message =
      case message of
        "ALPHABET" -> handleAlphabet
        ('M' : 'Q' : _) -> handleMQ message
        ('E' : 'Q' : _) -> handleEQ message
        _ -> do
          hPutStrLn stderr "Invalid command"
          exitFailure

    handleAlphabet = answer alphStr

    handleMQ str = do
      let (MQ word, _) = fromStr str
          acc = fun word
      answer $ if acc then "Y" else "N"

      modifyIORef countMQ succ
      n <- readIORef countMQ
      log $ "MQ " <> show n <> ": " <> str <> " -> " <> show acc -- <> " (parsed as " <> show word <> ")"

    handleEQ str = do
      modifyIORef countEQ succ
      n <- readIORef countEQ
      log $ "EQ " <> show n <> ": " <> str

      -- Currently, we handle equivalence queries very lazily: we simply pose
      -- a possible counterexample to the learner (which it might actually do
      -- correctly). There is no guarantee is will work.
      possibleCexes <- readIORef cexChecks
      case possibleCexes of
        [] -> do
          log " -> Y"
          answer "Y"
        (c : cs) -> do
          log $ " -> N " <> toStr c
          -- TODO: make this syntax the same as for MQs
          answer $ "N " <> toStr c
          writeIORef cexChecks cs

  -- Lazily answer all queries, until the stream is closed
  messages <- lines <$> getContents
  mapM_ act messages

  -- Then output some statistics
  m <- readIORef countMQ
  e <- readIORef countEQ
  hPutStrLn stderr $ "Total number of MQ: " <> show m
  hPutStrLn stderr $ "Total number of EQ: " <> show e

-- examples from https://gitlab.science.ru.nl/moerman/nominal-learning-ons/-/blob/master/examples.hpp
-- Note: we do not implement them as state machines. Instead we implement them
-- as functions, to really make the point that this is "black box learning".

fifoFun :: Int -> [FifoA] -> Bool
fifoFun n = acc . foldl' step (Just [])
 where
  step Nothing _ = Nothing
  step (Just ls) (Put a)
    | length ls >= n = Nothing
    | otherwise = Just (ls <> [a])
  step (Just []) (Get a) = Nothing
  step (Just (x : xs)) (Get a)
    | a == x = Just xs
    | otherwise = Nothing
  acc = isJust

fifoCex :: Int -> [[FifoA]]
fifoCex n = concatMap dw [1 .. n]
 where
  dw k = [fmap Put w <> fmap Get w | w <- OrbitList.toList (OrbitList.repeatRationals k)]

doubleFun :: Int -> [Atom] -> Bool
doubleFun 0 w = w == []
doubleFun n w = let (l, r) = splitAt n w in w /= [] && l == r

doubleCex :: Int -> [[Atom]]
doubleCex n = [w <> w | w <- OrbitList.toList (OrbitList.repeatRationals n)]
