import           Data.Monoid
import qualified Data.Map                      as Map

data RobotPart = RobotPart {
 name :: String,
 description :: String,
 cost :: Double,
 count :: Int
} deriving Show

type Html = String
renderHtml :: RobotPart -> Html
renderHtml part = mconcat
    [ "<h2>"
    , partName
    , "</h2>"
    , "<p>"
    , partDesc
    , "</p>"
    , "<p>"
    , partCost
    , "</p>"
    , "<p>"
    , partCount
    , "</p>"
    ]
  where
    partName  = name part
    partDesc  = description part
    partCost  = show (cost part)
    partCount = show (count part)

leftPart :: RobotPart
leftPart = RobotPart { name        = "left arm"
                     , description = "left arm for face punching!"
                     , cost        = 1000.0
                     , count       = 3
                     }

rightArm :: RobotPart
rightArm = RobotPart { name        = "right arm"
                     , description = "right arm for kind hand gestures"
                     , cost        = 1025.0
                     , count       = 5
                     }

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys    = [1 .. 3]
    vals    = [leftPart, rightArm]
    keyVals = zip keys vals

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

showParts :: [Html]
showParts = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftPart

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO
