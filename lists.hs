myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (l:[]) = l
myLast (h:tail) = myLast tail

myLastButOne :: [a] -> a
myLastButOne [l, lst] = l
myLastButOne (h:tail) = myLastButOne tail

elAt :: [a] -> Int -> a
elAt lst 1 = head lst
elAt lst idx = elAt (tail lst) (idx - 1)

lgt :: [a] -> Int
lgt [] = 0
lgt (x:[]) = 1
lgt l = 1 + (lgt (tail l))

butLast :: [a] -> [a]
butLast (e:[]) = []
butLast (h:t) = [h] ++ butLast t
butLast [] = []

rev :: [a] -> [a]
rev [] = []
rev l = [myLast l] ++ rev (butLast l)

isPalindrome s = rev s == s

compress :: String -> String
compress = compress' ""

compress' :: String -> String -> String
compress' [] s = compress' ([s!!0]) (tail s)
compress' r (s:t)
  | (myLast r) == s = compress' r t
  | otherwise = compress' (r ++ [s]) t
compress' r [] = r

pack :: [String] -> String -> [String]
pack acc [] = acc
pack [] (h:t) = pack [[h]] t
pack acc (h:t)
  | (last acc)!!0 == h  = pack ((butLast acc) ++ [((myLast acc) ++ [h])]) t
  | otherwise           = pack (acc ++ [[h]]) t

encode :: String -> [(Int, Char)]
encode s = map (\x -> (length x, (head x))) (pack [] s)

data CharsLst = Multiple Int Char | Single Char
  deriving (Show)

buildLstItem :: Char -> Int -> CharsLst
buildLstItem c 1 = Single c
buildLstItem c n = Multiple n c

encodemodified :: String -> [CharsLst]
encodemodified s = map (\x -> buildLstItem (head x) (length x)) (pack [] s)

decodemodified :: [CharsLst] -> String
decodemodified ((Single a):t) = [a] ++ decodemodified t
decodemodified ((Multiple n a):t) = (replicate n a) ++ decodemodified t
decodemodified [] = []

lengthencoding :: String -> [CharsLst]
lengthencoding = foldr lengthencoder []

lengthencoder :: Char -> [CharsLst] -> [CharsLst]
lengthencoder c (Multiple x a:t)
  | a == c = Multiple (x+1) a : t
  | otherwise = Single c : Multiple x a : t
lengthencoder c (Single a:t)
  | a == c = Multiple 2 a : t
  | otherwise = Single c : Single a : t
lengthencoder c [] = [Single c]

duplicate :: [a] -> [a]
duplicate = nplicate 2

nplicate :: Int -> [a] -> [a]
nplicate n l = foldl (\acc x -> acc ++ replicate n x) [] l

repli :: Int -> [a] -> [a]
repli n = nplicate n

dropnth :: Int -> [a] -> [a]
dropnth _ [] = []
dropnth n arr = take (n-1) arr ++ dropnth n (drop n arr)

split :: [a] -> Int -> ([a], [a])
split arr n = foldl (splitter n) ([], []) arr

splitter :: Int -> ([a], [a]) -> a -> ([a], [a])
splitter at acc x
  | length (fst acc) >= at = (fst acc, (snd acc) ++ [x])
  | otherwise = ((fst acc) ++ [x], snd acc)

slice :: [a] -> Int -> Int -> [a]
slice arr from to = (fst (split (snd (split arr (from - 1))) (to - from + 1)))

rotate :: [a] -> Int -> [a]
rotate arr n = drop (((length  arr) + n) `mod` (length arr)) arr ++ take (((length  arr) + n) `mod` (length arr)) arr

removeAt :: [a] -> Int -> [a]
removeAt arr n = take (n-1) arr ++ drop n arr

main = do
  putStrLn [myLast "ABCD"]
  putStrLn (butLast "ABCD")
  putStrLn [myLastButOne "ABCD"]
  putStrLn [elAt "ABCD" 3]
  print (lgt "AFDSA")
  print (lgt "")
  print (rev "ABC")
  print (isPalindrome "ABC")
  print (isPalindrome "ABCBA")
  putStrLn (compress "aaabbbbcccddd")
  putStrLn (compress "aaabbbbcccddd")
  putStrLn "......................"
  mapM putStrLn (pack [] "aaabbbbcccdddaaaaccccaaa")
  mapM print (encode "aaabcccdaaaaccccaaa")
  mapM print (encodemodified "aaabcccdaaaaccccaaa")

  putStrLn "aaabcccdaaaaccccaaa"
  putStrLn (decodemodified (encodemodified "aaabcccdaaaaccccaaa"))

  putStrLn "lengthencoding..."
  putStrLn "aaabcccdaaaaccccaaa"
  mapM print (lengthencoding "aaabcccdaaaaccccaaa")

  putStrLn (duplicate "abbcd")
  putStrLn (repli 3 "abbcd")

  putStrLn ( dropnth 3 "abcdefghik")

  print ( split "abcdefghik" 3)

  print ( slice "abcdefghik" 3 7)

  print ( rotate "abcdefgh" 3)

  print ( rotate "abcdefgh" (-2))

  print ( removeAt "abcd" 2)
