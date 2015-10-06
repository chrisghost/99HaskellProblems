not' True = False
not' False = False

and' True True = True
and' False _ = False
and' _ False = False

or' True _ = True
or' _ True = True

nand' a b = not' (and' a b)

nor' a b = not' (or' a b)

xor' True False = True
xor' False True = True
xor' _ _ = False

impl' True False = False
impl' _ _ = True

eq' True True = True
eq' False False = True
eq' _ _ = False

-- As operators

infixl 4 `or'`
infixl 6 `and'`

gray :: Int -> [String]
gray n = foldl gray' ["0", "1"] [2..n]

gray' :: [String] -> Int -> [String]
gray' s _ = map ('0':) s ++ map ('1':) (reverse s)

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table exp = [ (a, b, exp a b) | a <- [True, False], b <- [True, False]]

main = do
  mapM print (table (\a b -> (and' a (or' a b))))
  putStrLn "...................."
  mapM print (table (\a b -> a `and'` (a `or'` not b)))

  mapM print (gray 4)

