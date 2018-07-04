-- get user input character, print it out, use the IO monad correctly

main :: IO ()
main = getLine >>= (\c -> putStrLn c)
