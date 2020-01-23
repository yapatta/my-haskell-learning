import qualified Data.Map                      as Map

type GamerId = Int
type UserName = String
type Credits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList
    [(1, "gfdsd"), (2, "itaejkr"), (3, "rtjeltnr"), (4, "rteagtreh5")]

creditsDB :: Map.Map UserName Credits
creditsDB = Map.fromList
    [("rtsehrgw", 20000), ("itaejkr", 12000), ("rteagtreh5", 323000)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe Credits
lookupCredits username = Map.lookup username creditsDB

creditsFromId :: GamerId -> Maybe Credits
creditsFromId id = (>>=) (lookupUserName id) lookupCredits

echo = putStrLn
