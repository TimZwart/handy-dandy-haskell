module HandyDandy.Handydandy (map_with_index, splitLines, lookUp, has_number_of_elements,(!!!),replace, replace_assoc,list_to_2tuple, csv_to_assoc_list, splitOnNonQuoted, json_object_to_alist) where

import Data.Maybe
import Data.List

--map variant which allows you to use the index
map_with_index func xs = mapper 0 func xs
    where mapper i f (x:xs) = (f x i) :(mapper (i+1)  f xs)
          mapper i f [] = []

--universal newline from RWH
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

--useful for association lists
lookUp a assocs =
  errorspitter(lookup a assocs)
  where errorspitter (Just a) = a
        errorspitter Nothing = error ("E: "++(show a)++" is no key of alist "++(show assocs))
        
--instead of checking length, check just n elements in a list
has_number_of_elements :: Integral a=> a->[b]->Bool
has_number_of_elements n (x:xs) = if n>1 
                                  then has_number_of_elements (n-1) xs
                                  else True
has_number_of_elements _ [] = False

--(!!!) :: (Show a,Integral b)=> [a]->b->a
infixl 9 !!!
as !!! y=f as y 
    where f (x:xs) b= if b==0
                      then x
                      else f xs (b-1)
          f [] _= error ("!!!: list "++(show as)++" has less than "++(show y)++" elements")

--replace in a list
replace :: Eq a => a -> a -> [a] -> [a]
replace old new list = take pos list ++ new : drop(pos+1) list
    where pos = fromJust (elemIndex old list)

--replace in an association list
replace_assoc ::  (Eq a, Eq b) => a -> b -> [(a,b)] -> [(a,b)]
replace_assoc key new assoc = replace elem (key, new) assoc
    where --elem :: (a,b)
          elem = head (filter (sameKey) assoc)
          --sameKey :: (a,b) -> Bool
          sameKey (k,value) = k==key
 
list_to_2tuple :: [a]->(a,a)
list_to_2tuple xs = (xs!!0,xs!!1)

csv_to_assoc_list :: String -> [[(String,String)]]
csv_to_assoc_list rawCSV = map (zip firstLineList) restLineLists
    where lineCSV = splitLines (filter (/='"') rawCSV)
          restLineLists = map splitKom (tail lineCSV)
          firstLineList = splitKom (head lineCSV)

json_object_to_alist :: String -> [(String,String)]
json_object_to_alist rawJSONobject = map (list_to_2tuple . (map (filter (/='\"'))) . (splitOnNonQuoted ':')) (splitKom (filter notabrace rawJSONobject))
    where notabrace char = char /= '{' && char /= '}'

splitOnNonQuoted char str = splitter str [] "" False 
  where splitter (curr:rest) luc currs_uc quoted
         |curr=='\"'    = splitter rest luc (curr:currs_uc) (not quoted)
         |curr==char    =  if not quoted
                           then splitter rest ((reverse currs_uc):luc) "" quoted
                           else splitter rest luc (curr:currs_uc) quoted
         |otherwise     = splitter rest luc (curr:currs_uc) quoted
        splitter [] list laststring quoted = reverse ((reverse laststring):list)


