import Data.Char

encode :: Int -> String -> String
encode offset = map $ chr . (+offset) . ord

decode :: Int -> String -> String
decode shift = encode (negate shift)
