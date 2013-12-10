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
