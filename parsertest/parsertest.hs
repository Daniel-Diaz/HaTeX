
import Text.LaTeX
import Text.LaTeX.Base.Parser
import qualified Data.Text.IO as T

testNumbers :: [Int]
testNumbers = [1 .. 3]

testFile :: Int -> IO Bool
testFile i = do
  putStr $ "Parsing example " ++ show i ++ "... "
  t <- T.readFile $ "example" ++ show i ++ ".tex"
  case parseLaTeX t of
    Left err -> do putStrLn "Failed."
                   putStrLn $ "The error was: " ++ err
                   return False
    Right _  -> putStrLn "Succeed." >> return True

main :: IO ()
main = do
  putStrLn "Running Parser Test..."
  bs <- mapM testFile testNumbers
  let b = and bs
  putStrLn $ "Parser Test Passed: " ++ show b
  if b then return ()
       else putStrLn $ "Number of errors: " ++ show (length $ filter (==False) bs)
