
import Groningen

fib :: Groningen
fib = Groningen.fix 1 $ \x -> do
  y <- Groningen.delay 0 x
  return $ x + y

fibCode = runGroningen fib
fibSym = take 10 $ eval fibCode

shift123 :: Groningen
shift123 = Groningen.fix 1 $ \x -> do
  y <- Groningen.delay 3 x
  z <- Groningen.delay 2 y
  return z

shift123Code = runGroningen shift123
shift123Sym = take 10 $ eval shift123Code

main :: IO ()
main = return ()

