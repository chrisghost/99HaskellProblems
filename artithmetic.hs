import Data.Maybe

isprime :: Int -> Bool
isprime n = (length [x | x <- [2..n-1], n `mod` x == 0 ] ) == 0

myGcd :: Int -> Int -> Int
myGcd x 0 = x
myGcd x y = gcd y (x `mod` y)

coprime x y = (gcd x y) == 1

totient :: Int -> Int
totient n = length [x | x <- [1..n-1], coprime x n]

prime :: Int -> Int
prime n = prime' n 2

prime' :: Int -> Int -> Int
prime'  n d
  | n `mod` d == 0 = d
  | otherwise = prime' n (d+1)

headOption :: [a] -> Maybe a
headOption [] = Nothing
headOption (x:_) = Just x

primes :: Int -> [Int]
primes 1 = []
primes n = [prime n] ++ (primes (n `quot` (prime n)) )

primesR :: Int -> Int -> [Int]
primesR x y = [l | l <- [x..y], isprime l]

goldbach = fromJust . goldbach' 2

goldbach' :: Int -> Int -> Maybe (Int, Int)
goldbach' min x = headOption [ (l, m) | l <- (primesR min x), m <- (primesR min x), l + m == x]

goldbachlist :: Int -> Int -> [(Int, (Int, Int))]
goldbachlist from to = map (\x -> (x, goldbach x) ) (filter even [from..to])

goldbachlist' :: Int -> Int -> Int -> [(Int, (Int, Int))]
goldbachlist' from to m = flatten ( map (\x -> case (goldbach' m x) of
                                        Just i -> (Just (x, i))
                                        Nothing -> Nothing) (filter even [from..to])
                                        )

flatten :: [Maybe x] -> [x]
flatten l = map fromJust (filter isJust l)

main = do
  print (isprime 4)
  print (isprime 7)

  print (myGcd 36 63)
  print (myGcd (-3) (-6))
  print (myGcd (-3) 6)

  print (coprime 35 64)

  print (totient 10)

  print (primes 315)

  print (primesR 10 20)

  print (goldbach 28)

  print (goldbachlist 9 20)

  mapM print (goldbachlist' 1 2000 50)

