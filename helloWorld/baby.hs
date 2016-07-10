-- main :: IO()
main = do putStrLn "Hello, world."

doubleMe x = 2*x
doubleSmall x = (if x < 100 then x*2 else x)

take 50 (cycle [1,2,3]) -- [1,2,3,1,2,3...] len 50
replicate 3 10 -- [10,10,10]
xs = [x**2 | x <- [1,2..5], x**2 > 10]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
