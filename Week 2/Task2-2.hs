{-|

Write a function that, given a list of strings xs, creates a list of all two-character strings that can be made from characters
that appear in at least one of the strings in xs. 
E.g. if xs = ["no","way"] then the function returns 
["yy","ya","yw","yo","yn","ay","aa","aw","ao","an","wy","wa","ww","wo","wn","oy","oa","ow","oo","on","ny","na","nw","no","nn"] 
where the order is not important.

-}

allNCharStrings [] _ _ = []
allNCharStrings _ 0 _ = []
allNCharStrings (x:xs) n f
                            | f stringIncludingX = stringIncludingX : stringExcludingX
                            | otherwise = stringExcludingX
                            where
                                stringIncludingX = x : allNCharStrings xs (n-1) f
                                stringExcludingX = allNCharStrings xs (n-1) f