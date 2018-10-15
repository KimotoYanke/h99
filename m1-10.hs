module M0110
  ( encode
  )
where
import           Prelude

myLast :: [a] -> a
myLast (x : []) = x
myLast (x : xs) = myLast xs

myButLast :: [a] -> a
myButLast [x, _]   = x
myButLast (x : xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x : xs) 1 = x
elementAt (x : xs) n = elementAt xs (n - 1)

myLength :: [a] -> Int
myLength []       = 0
myLength (x : xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = (myReverse xs) ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []       = True
isPalindrome (_ : []) = True
isPalindrome (x : xs) = if x == y then isPalindrome ys else False
  where (y : ys) = myReverse xs

data NestedList a = Elem a | List [NestedList a] deriving Show
flatten :: NestedList a -> [a]
flatten (List []      ) = []
flatten (Elem x       ) = [x]
flatten (List (x : xs)) = flatten x ++ flatten (List xs)

compress :: Eq a => [a] -> [a]
compress []       = []
compress [x     ] = [x]
compress (x : xs) = if x /= y then x : compress xs else compress xs
  where (y : ys) = xs

pack :: Eq a => [a] -> [[a]]
pack []       = []
pack [x     ] = [[x]]
pack (x : xs) = if x == y
  then (x : packedXsHead) : packedXsTail
  else pack [x] ++ packedXs
 where
  packedXs                    = pack xs
  packedXsHead : packedXsTail = packedXs
  y            : yTail        = packedXsHead

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

main = do
  print $ "Problem 1"
  print $ myLast [1, 2, 3, 4]
  print $ myLast ['x', 'y', 'z']

  print $ "Problem 2"
  print $ myButLast [1, 2, 3, 4]
  print $ myButLast ['a' .. 'z']

  print $ "Problem 3"
  print $ elementAt [1, 2, 3] 2
  print $ elementAt "haskell" 5

  print $ "Problem 4"
  print $ myLength [123, 456, 789]
  print $ myLength "Hello, world!"

  print $ "Problem 5"
  print $ myReverse "A man, a plan, a canal, panama!"
  print $ myReverse [1, 2, 3, 4]

  print $ "Problem 6"
  print $ isPalindrome [1, 2, 3]
  print $ isPalindrome "madamimadam"
  print $ isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1]

  print $ "Problem 7"
  print $ flatten (Elem 5)
  print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
  print $ (flatten (List []) :: [Int])

  print $ "Problem 8"
  print $ compress "aaaabccaadeeee"

  print $ "Problem 9"
  print $ pack
    ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

  print $ "Problem 10"
  print $ encode "aaaabccaadeeee"

