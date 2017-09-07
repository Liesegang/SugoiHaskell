import Tree
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

data Probably a = None | Bang a

instance (Eq m) => Eq (Probably m) where
  Bang x == Bang y = x == y
  None == None = True
  _ == _ = False

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False

instance YesNo (Tree a) where
  yesno (Node _ _ _) = True
  yesno EmptyTree = False

instance YesNo (Probably a) where
  yesno (Bang _) = True
  yesno None = False

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf c t f = if yesno c then t else f
