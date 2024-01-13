
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c chars = head ([b | (a, b) <- chars, a == c] ++ [c])