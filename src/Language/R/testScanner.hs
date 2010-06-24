import Scanner
import System.Environment

main = do
  args <- getArgs
  let infile = args !! 0
  text <- readFile infile
  let toks = runScanner text
  mapM (putStrLn . show) toks
  