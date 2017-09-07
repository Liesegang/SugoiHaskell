import qualified Data.Map as Map
import qualified Data.Char as Char

phones = [("betty", "390809")
    ,("betty", "74509432")
    ,("bonnie", "93850980")
    ,("pasty", "398590")
    ,("pasty", "5748392")
    ,("lucille", "84930")
    ,("wendy", "385023")
    ,("penny", "3905809")]

phoneBook :: Map.Map String String
phoneBook = Map.fromList phones

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith add
    where add x y = x ++ "," ++ y

string2digits :: String -> [Int]
string2digits = map Char.digitToInt . filter Char.isDigit


