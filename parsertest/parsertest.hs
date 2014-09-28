
import Text.LaTeX
import Text.LaTeX.Base.Parser
import qualified Data.Text.IO as T
import System.Exit (exitSuccess, exitFailure)

testNumbers :: [Int]
testNumbers = [1 .. 5]

testFile :: Int -> IO Bool
testFile i = do
  putStr $ "Parsing example " ++ show i ++ "... "
  t <- T.readFile $ "parsertest/example" ++ show i ++ ".tex"
  case parseLaTeX t of
    Left err -> do putStrLn "Failed."
                   putStrLn $ "The error was: " ++ show err
                   return False
    Right _  -> putStrLn "Succeed." >> return True

main :: IO ()
main = do
  putStrLn "Running Parser Test..."
  bs <- mapM testFile testNumbers
  let b = and bs
  putStrLn $ "Parser Test Passed: " ++ show b
  if b then exitSuccess
       else do putStrLn $ "Test result: " ++ show (length $ filter (==True) bs) ++ "/" ++ show (length bs)
               exitFailure
