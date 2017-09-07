import qualified Data.Map as Map

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector x1 x2 x3) (Vector y1 y2 y3) = Vector (x1 + y1) (x2 + y2) (x3 + y3)

dotprod :: (Num a) => Vector a -> Vector a -> a
dotprod (Vector x1 x2 x3) (Vector y1 y2 y3) = x1 * y1 + x2 * y2 + x3 * y3

data Person = Person { firstName :: String
                     , lastName :: String
                     , age ::Int
                     } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thirsday | Friday | Saturday | Sunday
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneNumber :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneNumber name phoneNumber phoneBook = (name, phoneNumber) `elem` phoneBook

type AssocList k v = [(k, v)]
type IntMap = Map.Map Int


