import AbstractSet
import qualified Set as S
import qualified TreeSet as TS
import System.Random

randList :: IO [Int]
randList = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  let cnt = head $ (randomRs (1,20) gen1 :: [Int])
  return $ take cnt $ (randomRs (0,100) gen2 :: [Int])


toSet :: AbstractSet s => s -> [Int] -> s
toSet s xs = foldl (add) s xs


checkSet1 :: AbstractSet s => s -> Bool
checkSet1 s = contains (add (add s 1) 2) 1


checkSet2 :: AbstractSet s => s -> [Int] -> Bool
checkSet2 s xs = not $ contains s' (head xs + 101)
 where
  s' =  toSet s xs


checkSet3 :: AbstractSet s => s -> [Int] -> Bool
checkSet3 s xs = contains s' (last xs)
 where
  s' = toSet s xs


main = do
	xs <- randList
	print $ checkSet1 (add empty 10 :: S.Set) && checkSet1 (add empty 10 :: TS.Set)
	print $ checkSet2 (empty :: S.Set) xs && checkSet2 (empty :: TS.Set) xs
	print $ checkSet3 (empty :: S.Set) xs && checkSet3 (empty :: TS.Set) xs
