--bubblesort
bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x]= [x]
bubble (x:y:xs)
    | x > y     = y : bubble (x:xs)
    | otherwise = x : bubble (y:xs)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort x
    | x == y = x
    | otherwise = bubbleSort y
    where y = bubble x

--returns true if the first argument is a prefix of the second and false otherwise
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys)
  | x == y    = isPrefix xs ys
  | otherwise = False

--returns true if the first argument is a substring of the second
isSubstring :: String -> String -> Bool
isSubstring [] _ = True
isSubstring _ [] = False
isSubstring x y =  isPrefix x y || isSubstring x (tail y)

--generates all the tails of the given list, so that e.g., genTails "hey" = ["hey","ey","y"]. genPrefix helper function
genTails :: String -> [String]
genTails "" = []
genTails x = x : genTails (tail x)

--generates all non-empty prefixes of the given string and puts them in a list
genPrefix :: String -> [String]
genPrefix "" = []
genPrefix x = reverse (map reverse (genTails (reverse x)))

--generates all the substrings of a given string and puts them in a list.
genSubstrings :: String -> [String]
genSubstrings "" = [""]
genSubstrings x = genSubstrings (tail x) ++ genPrefix x 

--takes a pair (old,new) of strings, another string str, and replaces the prefix old of str with the string new.
replacePrefix :: (String,String) -> String -> String
replacePrefix (_,_) "" = ""
replacePrefix (a,b) y 
    | not(isPrefix a y)  = ""
    | otherwise          = b ++ drop (length a) y


--replaces the first occurrence of a substring with a new string
replaceString :: (String,String) -> String -> String
replaceString (_,_) "" = ""
replaceString (a,b) y
    | isPrefix a y  = replacePrefix (a, b) y 
    | otherwise     = head y : replaceString (a, b) (tail y)

--cypher----------------

--calling lookUp x perm searches the list perm for the pair (x,y) whose first coordinate
--is the given character x. It should then output the second coordinate y.
lookUp :: Char -> [(Char,Char)] -> Char
lookUp y [] = y
lookUp y ((a, b):xs) 
    | y == a    = b
    | otherwise = lookUp y xs


--takes a lookup table and a string and replaces every character by the value it is mapped to by the given table.
encode :: [(Char,Char)] -> String -> String
encode _ "" = ""
encode table (x:xs) = lookUp x table : encode table xs


--takes two strings and creates a table by pairing up their characters at the same position.
makeTable :: String -> String -> [(Char,Char)]
makeTable "" "" = []
makeTable (x:xs) (y:ys) = (x,y) : makeTable xs ys

--caesar cypher
caesar :: Int -> [(Char,Char)]
caesar n = (' ',' ') : makeTable abc (drop n (cycle abc)) where abc = ['A'..'Z']