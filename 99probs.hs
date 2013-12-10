-- 99 Haskel problems
-- http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems
-- q2 Find all but 2nd last element in a list

myButLast :: [a]->a
--myButLast a:b:xs
--        | [] = []
--        | a:b:[] = a
--        | a:b:xs = myButLast xs
myButLast [] = error "not big enough"
myButLast [a,b] = a
myButLast a = myButLast (tail a)

-- q3 Find the kth element of a list, first element is 1
elementAt :: [a]->Int->a
elementAt xs i = xs !! (i-1)

-- q4 Find the number of elements in a list
myLength :: [a]->Int
myLength = length

-- q5 Reverse a list
myReverse :: [a]->[a]
myReverse = reverse

-- q6 determine if a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs
            | null xs = True
            | length xs ==1 = True
            | (head xs) == (last xs) = isPalindrome (tail (init xs))
            | otherwise = False

-- q7 flatten a nested list into it's elements
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a-> [a]
flatten (Elem a) = [a]
flatten (List (a:nl)) = (flatten a) ++ (flatten (List nl))
flatten (List []) = []

--q8 eliminate consecutive duplicates in a list
--compress ::(Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = cmp xs ([]:x) x
        where cmp xs@(y:ys) accum id
                | null xs       = accum
                | y==id         = cmp ys accum id
                | otherwise     = cmp ys ((head x):accum) x
-- above solution not correct, try another
compress' :: [a] -> [a]
compress' (x:ys@(y:_))
        | x == y    = compress ys
        | otherwise = x : compress ys
compress' ys = ys
-- Ok well that was copy-pasting the answer and it still
-- won't work... So I'm stumped
