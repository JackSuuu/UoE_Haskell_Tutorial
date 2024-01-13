mergesort :: (Ord k) => [(k, a)] -> [(k, a)]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = m (mergesort left) (mergesort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

m :: (Ord k) => [(k, a)] -> [(k, a)] -> [(k, a)]
m [] ys = ys
m xs [] = xs
m left@(x:xs) right@(y:ys)
    | fst x <= fst y = x : m xs right
    | otherwise      = y : m left ys

-- toListDom :: Keymap k a -> [(k,a)]
-- toListDom t = go t []
--     where
--       go :: Keymap k a -> [(k,a)] -> [(k,a)]
--       go Leaf acc = acc
--       go (Node k v left right) acc = go left ((k,v) : go right acc)
