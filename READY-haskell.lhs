-- 5
get_chunk_len (f:[]) = 1
get_chunk_len (f:s:r) =
  if (s /= f)
  then 1
  else 1 + get_chunk_len (s:r)

remove_chunk (f:[]) = []
remove_chunk (f:s:r) =
  if (s /= f)
  then s:r
  else remove_chunk (s:r)

compress [] = []
compress (lst) = [(get_chunk_len lst, head lst)] ++ compress (remove_chunk lst)


-- 18

delete_from lst el = 
  if null lst
  then lst
  else
    if (head lst) == el
    then tail lst
    else [head lst] ++ (delete_from (tail lst) el)

compare_lists lst1 lst2 =
  if (length lst1) /= (length lst2)
    then False
    else 
      if (length lst1) /= 0
      then compare_lists (delete_from lst1 (head lst1)) (delete_from lst2 (head lst1))
      else True
        
-- 20

member lst el = 
  if null lst
  then False
  else 
    if (head lst) == el
    then True
    else member (tail lst) el

is_not_cross [] _ = True
is_not_cross lst1 lst2 = do
  let m = head lst1
  if (member lst2 m)
  then False
  else is_not_cross (tail lst1) lst2


-- 23

xor lst1 lst2 =
  if null lst1
  then lst2
  else do
    let m = head lst1
    if (member lst2 m)
    then xor (tail lst1) (delete_from lst2 m)
    else [m] ++ ( xor (tail lst1) lst2 )


--

-----------------
--onion n =
--  make_onion n 1

--make_onion x y =
--  if x == y
--    then [x]
--    else do
--    let z = y+1
--    [(make_onion (x z))]

--main = do
-- print (onion 1)

main = do
  print ( "task 5:" )
  print (compress ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])
  print (compress ['a', 'b', 'c', 'a', 'd', 'e'])
  print ( "----------" )
  print( "task 18" )
  print (compare_lists [1..6] [1..6])
  print (compare_lists [1..6] [2..7])
  print ( "----------" )
  print( "task 20" )
  print (is_not_cross [1..10] [10..20])
  print (is_not_cross [1..10] [11..20])
  print ( "----------" )
  print( "task 23" )
  print (xor [1..20] [11..30])
  print (xor [1..20] [1..20])