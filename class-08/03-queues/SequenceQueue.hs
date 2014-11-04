module SequenceQueue (Queue, empty, enqueue, dequeue, isEmpty) where

import AbstractQueue
import qualified Data.Sequence as Seq

newtype Queue t = QueueImpl (Seq.Seq t)

instance AbstractQueue Queue where
  empty = QueueImpl Seq.empty

  isEmpty (QueueImpl xs) = Seq.null xs

  enqueue (QueueImpl xs) x = QueueImpl (x Seq.<| xs)

  dequeue (QueueImpl xs) = (x, QueueImpl q)
    where
      (q Seq.:> x) = Seq.viewr xs
