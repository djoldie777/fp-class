{-
3. Пользуясь монадой State, реализовать функции для работы с очередью: enqueue и dequeue.
-}

import Control.Monad.State

type Queue = [Int]

enqueue :: Int -> State Queue ()
enqueue x = do
  xs <- get
  put (xs ++ [x])

dequeue :: State Queue Int
dequeue = do
  (x:xs) <- get
  put xs
  return x

queueManip :: State Queue Int
queueManip = do
  enqueue 7
  enqueue 4
  enqueue 5
  a <- dequeue
  dequeue

test = runState queueManip [] == (4, [5])