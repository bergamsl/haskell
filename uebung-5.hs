import Data.Monoid

data MyList a = Leer | Element a (MyList a) deriving Show

class ToDouble a where
  toDouble :: a -> Double

instance ToDouble (MyList a) where
  toDouble Leer = 0
  toDouble (Element a b) = 1 + (toDouble b)


instance Eq a => Eq (MyList a) where
  (==) Leer Leer = True
  (==) (Element x y) (Element z g) = x == z && y == g
  (==) _ _ = False

-- Marktschreier

data Sordde = Erwel | Gummern deriving (Eq, Show)
data Dudde = Dudd { sort:: Sordde, kilo :: Double, duddenpreis:: Double}
            | Gemischde { kilo :: Double, kilopreis :: Double} deriving Show



instance Monoid (Dudde) where
  mempty = Gemischde 0 0
  mappend dude1 dude2
    | kilo dude1 <= 0 = dude2
    | kilo dude2 <= 0 = dude1
  mappend (Dudd s1 k1 p1) (Dudd s2 k2 p2)
    | s1 == s2 = Dudd s1 (k1 + k2) (p1 + p2)
  mappend (Gemischde k1 p1) (Gemischde k2 p2) =
      Gemischde (k1 + k2) ((k1*p1+k2*p2) / (k1 + k2))
  mappend d1 d2 = mappend (mische d1) (mische d2)
    where
      mische :: Dudde -> Dudde
      mische (Dudd s k p) = Gemischde {kilo = k, kilopreis = (p/k)}
      mische sonstiges = sonstiges
