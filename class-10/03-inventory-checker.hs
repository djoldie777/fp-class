import Control.Monad
import Data.List
import System.Environment
import Data.Maybe

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType 
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)


loadInventory :: FilePath -> IO [ArmorItem]
loadInventory fname = (readFile fname) >>= return . liftM (\(k, t) -> ArmorItem (read k) (read t)) . liftM (\x -> span (/= ' ') x) . lines


buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit kind items = if (length $ nub types) == 5 then Just (ArmorKit kind types) else Nothing
  where
    types = liftM (\(ArmorItem _ t) -> t) $ filter (\(ArmorItem k _) -> k == kind) items


buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits items = sequence $ filter (isJust) list
  where
    list = zipWith buildArmorKit [Chitin, Hide, Leather, Elven, Scaled, Glass, ImperialLight] (replicate 7 items)


main = (head `liftM` getArgs) >>= loadInventory >>= print . buildKits
