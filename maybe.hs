import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.List

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgans :: Organ -> [Maybe Organ] -> Int
countOrgans organ available = length (filter (\x -> x == Just organ) available)

{-
isSomething :: Maybe Organ -> Bool
isSomething = \x -> x /= Nothing
-- isSomething Nothing = False
-- isSomething (Just _) = True
-}

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isJust availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing      = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleenList :: String
cleenList = intercalate ", " organList

numOrZero :: (Maybe Int) -> Int
numOrZero Nothing  = 0
numOrZero (Just a) = a

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat    organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag    organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat    organ) = (Lab, Vat organ)
placeInLocation (Cooler organ) = (Lab, Cooler organ)
placeInLocation (Bag    organ) = (Kitchen, Bag organ)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) = show container ++ " in the " ++ show location

{-
report :: Maybe (Location, Container) -> String
report (Just (location, container)) = show container ++ " in the " ++ show location
report Nothing = "container not found"
-}

processAndReport :: (Maybe Organ) -> String
processAndReport Nothing      = "error, id not found"
processAndReport (Just organ) = report (process organ)

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
    where organ = Map.lookup id catalog
