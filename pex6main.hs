-- pex6.hs 
-- unKnot Haskell

-- name: Zarif Bin Mohd Zamrin

{- DOCUMENTATION: Referenced Haskell quick reference  as well as watched a video by Derek Banas to help understand Tuples and Elem functions. Discussed with C2C McBrayer with regards to the wrap around feature. I ended up giving up
                  and submitting this PEX without the wrap around feature. Did not refer to a picture of the solution I took during class because I deleted it right after I took it as well as not trying to cheat the teacher or myself.
}

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null (typeIIknot(typeIknot tripCode)) = "unknot" --managed to completely untangle everything, return unknot
   | typeIIknot (typeIknot tripCode) == tripCode =  "tangle - resulting trip code: " ++ (show tripCode) -- apply typeI then typeII and if nothing changes, its officially a knot
   | otherwise = unKnot (typeIIknot(typeIknot tripCode))

 

typeIknot :: [(Char,Char)]  -> [(Char,Char)]

typeIknot [] = []
typeIknot [x] = [x]

typeIknot ((c1,t1) : (c2,t2) : xs)
    | c1 == c2 && not (t1 == t2) = xs
    | otherwise = (c1,t1) : typeIknot ((c2,t2):xs) 



typeIIknot :: [(Char,Char)] -> [(Char,Char)]
typeIIknot tripcode
    | findOver tripcode == [] = tripcode
    | findUnder (fst(head(findOver tripcode))) (fst(head(tail(findOver tripcode))))  tripcode == [] = tripcode
    | otherwise = typeIIknot (remove (findOver tripcode) (findUnder (fst(head(findOver tripcode))) (fst(head(tail(findOver tripcode))))  tripcode) tripcode)   -- chain of procedures. removes pairs that are determined by findOver, findUnder finds the matching pairs for the findOver, and removes the pairs from the original tripcode



findOver :: [(Char,Char)] -> [(Char,Char)]
findOver [] = []
findOver [x] = []

findOver ((c1,t1):(c2,t2):xs)
    | t1 == 'o' && t2 == 'o'  =  [(c1,t1),(c2,t2)]
    | otherwise = findOver ((c2,t2):xs)

findUnder :: Char -> Char -> [(Char,Char)] -> [(Char,Char)]
findUnder a b [] = []
findUnder a b [x] = []
findUnder a b ((c1,t1):(c2,t2):xs)
    | t1 == 'u' && t2 == 'u' && ( (c1 == a && c2 == b) || (c1 == b && c2 == a) )  =  [(c1,t1),(c2,t2)]
    | otherwise = findUnder a b ((c2,t2):xs)


remove :: [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)]
remove overPair underPair tripcode
    | otherwise = removePairs tripcode (overPair ++ underPair)


removePairs :: [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)]  
removePairs [] x = []
removePairs (x:xs) removeItem
    | elem x removeItem = removePairs xs removeItem
    | otherwise = x : removePairs xs removeItem




main :: IO ()
main = do

--type I unknot (should be successfull unknot)

   let t01 = [('a','o'),('a','u')]
   print("Test case t01" )
   print(t01)
   print("Result: " ++ unKnot t01)

   let t02 = [('a','o'), ('a','u'), ('b','u'), ('b','o')]
   print("Test case t02" )
   print(t02)
   print("Result: " ++ unKnot t02)

--type II unknot (should be successfull unknot)

   let t03 = [('a','o'), ('b','o'), ('b','u'), ('a','u')]
   print("Test case t03" )
   print(t03)
   print("Result: " ++ unKnot t03)

   let t04 = [('a','o'),('b','o'),('c','o'),('c','u'),('b','u'),('a','u')]
   print("Test case t04" )
   print(t04)
   print("Result: " ++ unKnot t04)

   let t05 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("Test case t05" )
   print(t05)
   print("Result: " ++ unKnot t05)
