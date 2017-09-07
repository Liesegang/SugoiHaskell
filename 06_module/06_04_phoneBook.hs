phoneBook = 
  [("betty", "390809")
  ,("bonnie", "93850980")
  ,("pasty", "398590")
  ,("lucille", "84930")
  ,("wendy", "385023")
  ,("penny", "3905809")]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
--findKey key = snd . head . filter (\(k, v) -> k == key)
findKey key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing
