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
--compress' :: [a] -> [a]
--compress' (x:ys@(y:_))
--        | x == y    = compress ys
--        | otherwise = x : compress ys
--compress' ys = ys
-- Ok well that was copy-pasting the answer and it still
-- won't work... So I'm stumped

-- q9 pack consecutive elements into separate lists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:takeWhile(==x) xs):pack(dropWhile(==x) xs)
-- I want to try another solution
pack' :: (Eq a) => [a] ->[[a]]
pack' [] = []
pack' [x] = [[x]]
pack' (x:xs) = if x `elem` (head(pack xs))
		then (x:(head (pack xs))):(tail (pack xs))
		else [x]:(pack xs)

-- q10 encode list into run length encoding
encode :: (Eq a) => [a] -> [(Int,a)]
encode a = fnct x 
	  where x = pack a
		fnct [[]] = []
		fnct (y:ys) =(length y, head y):(fnct ys)
		fnct [] = []

-- q11 modified run length encoding
data RLencode a = Multiple Int a | Single a deriving Show
encodeModified :: (Eq a) => [a] -> [RLencode a]
encodeModified a = fnct q
            where q = encode a
                  fnct [] = []
                  fnct (x@(i, a):xs) = if i==1
                            then (Single a):fnct xs
                            else (Multiple i a):fnct xs
-- q12 decode run length from above
decodeModified :: [RLencode a] -> [a]
decodeModified [] = []
decodeModified ((Single c):cs) = c:decodeModified cs
decodeModified ((Multiple i c):cs) = (extend i c)++decodeModified cs
                            where   extend 0 c = []
                                    extend 1 c = [c]
                                    extend num c = c:extend (num-1) c 

-- q13 create run-length encoding without using sublists
-- I think this is what I did in q11
encodeDirect :: (Eq a) => [a] -> [RLencode a]
encodeDirect [] = []
encodeDirect (x:xs) = helper 1 x xs
        where   helper i n [] = [element i n]
                helper i n ns = if n == (head ns) 
                                then helper (i+1) n (tail ns)
                                else element i n:(helper 1 (head ns) (tail ns))
                element 1 p = Single p
                element z p = Multiple z p

-- q14 Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)
-- I was originally trying to do list comprehension-style
dupli' x = concat [[a,a] | a <- x]

-- q15 replicate the elements of a list x times
repli :: [a] -> Int -> [a]
repli list x = concatMap (replicate x) list

-- q16 drop every Nth element
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list num = helper list num num []
        where   helper [] _ orig accum = reverse accum
                helper (x:xs) 1 orig accum = helper xs orig orig accum
                helper (x:xs) i orig accum = helper xs (i-1) orig (x:accum)
 
-- q17 split a list into 2 parts, not using predefined predicates
split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split list num = helper list num []
            where   helper (x:xs) 1 accum = (reverse(x:accum), xs)
                    helper (x:xs) i accum = helper xs (i-1) (x:accum)
                    helper [] _ accum = ([],accum)

-- q18 extract a slice from a list (inclusive)
-- list indices start at 1
slice :: [a] -> Int -> Int -> [a]
slice a x y = take (y-x+1) $ drop (x-1) a

--q19 rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate a x
        |x == 0 = a
        |x > 0  = (drop x a) ++ (take x a)
        |x < 0  = rotate a ((length a) + x)

-- q20 remove the N'th element from a list 
removeAt :: Int -> [a] -> (a,[a])
removeAt x a = (a !! (x-1), resid x a [])
            where   resid 1 (y:ys) accum= (reverse accum)++ys
                    resid _ [] accum = accum
                    resid i (y:ys) accum= resid (i-1) ys (y:accum)

-- q21 insert an element into the given position
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = left ++ (x: right)
        where left = fst z
              right = snd z
              z = split xs $ i-1  

-- q22 create a list with all elements 
range :: Int -> Int -> [Int]
range a b | a<=b = [a..b]
          | otherwise = reverse $ range b a
