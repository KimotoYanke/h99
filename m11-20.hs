module M1120 where
import           Prelude

encode :: Eq a => [a] -> [(Int, a)]
encode []       = []
encode [x     ] = [(1, x)]
encode (x : xs) = if x == y
  then (n + 1, x) : encodedXsTail
  else encode [x] ++ encodedXs
 where
  encodedXs                     = encode xs
  encodedXsHead : encodedXsTail = encodedXs
  (n, y)                        = encodedXsHead

data ListItem a = Multiple Int a | Single a deriving Show
convertToListItem :: (Int, a) -> ListItem a
convertToListItem (1, x) = Single x
convertToListItem (n, x) = Multiple (n) x

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = map convertToListItem (encode xs)

decodeModified :: [ListItem a] -> [a]
decodeModified []         = []
decodeModified [Single x] = [x]
decodeModified [Multiple n x] =
  if n == 1 then [x] else x : decodeModified [Multiple (n - 1) x]
decodeModified (x : xs) = arr ++ decodeModified xs
  where arr = decodeModified [x]

increment :: ListItem a -> ListItem a
increment (Single x    ) = Multiple 2 x
increment (Multiple n x) = Multiple (n + 1) x

content :: ListItem a -> a
content (Single x    ) = x
content (Multiple _ x) = x

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect []       = []
encodeDirect [x     ] = [Single x]
encodeDirect (x : xs) = if x == y
  then (increment encodedXsHead) : encodedXsTail
  else (Single x) : encodedXs
 where
  encodedXs                     = encodeDirect xs
  encodedXsHead : encodedXsTail = encodedXs
  y                             = content encodedXsHead

dupli :: [a] -> [a]
dupli []       = []
dupli (x : xs) = [x, x] ++ dupli (xs)

times :: a -> Int -> [a]
times x 0 = []
times x n = x : times x (n - 1)

repli :: [a] -> Int -> [a]
repli []       _ = []
repli _        0 = []
repli (x : xs) n = (times x n) ++ repli xs n

dropEvery :: [a] -> Int -> [a]
dropEvery = dropEveryWithCounter 0
dropEveryWithCounter :: Int -> [a] -> Int -> [a]
dropEveryWithCounter _ [] _ = []
dropEveryWithCounter i (x : xs) n
  | (i + 1) `mod` n == 0 = dropEveryWithCounter (i + 1) xs n
  | otherwise            = x : dropEveryWithCounter (i + 1) xs n

split :: [a] -> Int -> ([a], [a])
split xs       0 = ([], xs)
split (x : xs) 1 = ([x], xs)
split arr      n = (xs ++ [y], ys)
 where
  backState      = split arr (n - 1)
  (xs, (y : ys)) = backState

slice :: [a] -> Int -> Int -> [a]
slice xs i k = result
 where
  (_     , rest) = split xs $ i - 1
  (result, _   ) = split rest $ k - i + 1

rotate :: [a] -> Int -> [a]
rotate xs i | i > 0     = let (x0, x1) = split xs i in x1 ++ x0
            | i < 0     = let (x0, x1) = split xs $ (length xs) + i in x1 ++ x0
            | otherwise = xs

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), (fst $ split xs $n-1) ++ (snd $ split xs n) )

main = do
  print $ "Problem 11"
  print $ encodeModified "aaaabccaadeeee"

  print $ "Problem 12"
  print $ decodeModified
    [ Multiple 4 'a'
    , Single 'b'
    , Multiple 2 'c'
    , Multiple 2 'a'
    , Single 'd'
    , Multiple 4 'e'
    ]

  print $ "Problem 13"
  print $ encodeDirect "aaaabccaadeeee"

  print $ "Problem 14"
  print $ dupli [1, 2, 3]

  print $ "Problem 15"
  print $ repli "abc" 3

  print $ "Problem 16"
  print $ dropEvery "abcdefghik" 3

  print $ "Problem 17"
  print $ split "abcdefghik" 3

  print $ "Problem 18"
  print $ slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7

  print $ "Problem 19"
  print $ rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] 3
  print $ rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] (-2)

  print $ "Problem 20"
  print $ removeAt 2 "abcd"
