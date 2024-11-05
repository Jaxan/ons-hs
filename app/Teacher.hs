import System.IO (hFlush, stderr, stdout, hPutStrLn)

-- TODO: Actually implement interesting languages... Not sure yet how I want
-- to do equivalence queries. Maybe let the teacher respond with a test
-- query to the learner?
main :: IO ()
main = do
  messages <- lines <$> getContents
  mapM_ act messages
 where
  act message =
    case message of
      "ALPHABET" -> handleAlphabet
      str -> case take 2 message of
        "MQ" -> handleMQ (drop 3 message)
        "EQ" -> handleEQ (drop 3 message)
  handleAlphabet = do
    putStrLn "ATOMS"
    hFlush stdout
  handleMQ str = do
    -- accepts any string
    putStrLn "Y"
    hPutStrLn stderr $ "MQ received: " <> str
    hFlush stdout
  handleEQ str = do
    -- immediately accepts the hypothesis
    putStrLn "Y"
    hPutStrLn stderr $ "EQ received: " <> str
    hFlush stdout
