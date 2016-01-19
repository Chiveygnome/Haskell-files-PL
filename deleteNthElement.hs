module DeleteNthElement where
deleteNthElement      :: Int -> [a] -> [a]
--deleteNthElement xs = []
help x xs             = take x xs
help' x xs            = drop (x+1) xs
deleteNthElement x xs = help x xs ++ help' x xs 