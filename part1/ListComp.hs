module ListComp where

-- [2*i | i<-[1,2,3]]
-- ==> [2,4,6]

-- [i | i <- [1..7], even i]
-- ==> [2,4,6]

-- [f x | x <- lis, p x] == map f (filter p lis)

-- [first ++ " " ++ last | first <- ["John", "Mary"], last <- ["Smith", "Cooper"]]
-- ==> ["John Smith","John Cooper","Mary Smith","Mary Cooper"]

-- [reversed | word <- ["this", "is"], let reversed = reverse word]
-- ==> ["siht", "si"]

firstLetters string = [char | (char:_) <- words string]