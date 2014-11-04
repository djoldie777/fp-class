import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import System.Random
import System.Environment
import qualified SequenceQueue as SQ

checkQueue :: (AbstractQueue q, Num a, Eq a) => q a -> Bool
checkQueue q = lastElem (enqueue q 5) == 5
 where
  lastElem q = let (x, q') = dequeue q in
               if isEmpty q' then x else lastElem q'


listLength :: (Eq a, Num a) => a -> a
listLength 1 = 1
listLength l = l + listLength (l - 1)


randList :: Int -> IO [Int]
randList n = do
  gen <- newStdGen
  return $ take n $ (randomRs (0,100) gen :: [Int])


toList :: AbstractQueue q => q a -> [a]
toList q
  | isEmpty q = []
  | otherwise = let (x, q') = dequeue q in
	x : toList q'


addRemove :: AbstractQueue q => q a -> [a] -> q a
addRemove q xs = rest (length xs) (foldl (enqueue) q xs)
 where
  rest 1 q = q
  rest n q = rest (n-1) (snd $ dequeue q)


repeatAction :: AbstractQueue q => q a -> [a] -> Int -> q a
repeatAction q xs n = repeatAction' q xs n 1
 where 
  repeatAction' q xs n tmp
    | n < 1 = error "There is no way for n to be negative or null"
    | n == 1 = addRemove q xs
    | tmp == 1 = repeatAction' (enqueue q $ head xs) (tail xs) n (tmp + 1)
    | tmp == n = addRemove q xs
    | otherwise = repeatAction' (addRemove q (take tmp $ xs)) (drop tmp xs) n (tmp + 1)


remainElems :: (Monad m, AbstractQueue q) => q a -> [a] -> Int -> m [a]
remainElems q xs n = return $ toList $ repeatAction q xs n


main = do
	(arg : _) <- getArgs
	n <- readIO arg
	xs <- randList $ listLength n
	q1 <- remainElems (empty :: Q.Queue Int) xs n
	q2 <- remainElems (empty :: FQ.Queue Int) xs n
	q3 <- remainElems (empty :: SQ.Queue Int) xs n
	putStrLn $ "Q : " ++ (show q1)
	putStrLn $ "FQ : " ++ (show q2)
	putStrLn $ "SQ : " ++ (show q3)
	print $ (q1 == q2 && q1 == q3 && length q1 == n)
