module Main

import Data.String

getInteger : IO Integer
getInteger = do
  putStr "guess a number: "
  n <- getLine
  case parsePositive n of
    Just n => pure n
    Nothing => do
      putStrLn $ n ++ " is not a number."
      getInteger


withOpenFile : String -> Mode -> (File -> IO (Either FileError a)) -> IO (Either FileError a)
withOpenFile filename mode f = do
  Right file <- openFile filename mode
    | Left e => pure (Left e)
  ret <- f file
  closeFile file
  pure ret


getRandom : IO (Either FileError Int)
getRandom = withOpenFile "/dev/random" Read fGetInt
where
  stringToInt : String -> Int
  stringToInt chars = foldl (\acc, int => (acc `shiftL` 8) + int) 0 $ map ord $ unpack chars

  fGetInt : File -> IO (Either FileError Int)
  fGetInt file = do
    Right chars <- fGetChars file 4
      | Left e => pure (Left e)
    pure $ Right $ stringToInt chars

game : Integer -> IO ()
game secret = do
  n <- getInteger
  case compare n secret of
    LT => do
      putStrLn "Too small"
      game secret
    EQ => putStrLn "You got it"
    GT => do
      putStrLn "Too big"
      game secret



main : IO ()
main = do
  Right int <- getRandom
    | pure ()
  game $ cast $ 1 + (int `mod` 100)
