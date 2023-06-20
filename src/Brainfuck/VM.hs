{-# LANGUAGE RankNTypes #-}

module Brainfuck.VM (VMState, runWithVMState, Brainfuck.VM.read, modify, write, shiftIdx) where

import Control.Monad.ST (ST)
import qualified Control.Monad.ST as ST
import Data.STRef (STRef)
import qualified Data.STRef as ST
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8)

data VMState s = VMState {_memory :: MV.STVector s Word8, _index :: STRef s Int}

_memorySize :: Int
_memorySize = 1024

runWithVMState :: (forall s. VMState s -> ST s a) -> a
runWithVMState withState =
  ST.runST $ do
    memory <- MV.replicate _memorySize 0
    index <- ST.newSTRef 0
    let newState = VMState {_memory = memory, _index = index}
    withState newState

read :: VMState s -> ST s Word8
read vmState = do
  index <- ST.readSTRef (_index vmState)
  MV.unsafeRead (_memory vmState) index

modify :: VMState s -> (Word8 -> Word8) -> ST s ()
modify vmState modifier = do
  index <- ST.readSTRef (_index vmState)
  MV.unsafeModify (_memory vmState) modifier index

write :: VMState s -> Word8 -> ST s ()
write vmState value = do
  index <- ST.readSTRef (_index vmState)
  MV.unsafeWrite (_memory vmState) index value

shiftIdx :: VMState s -> (Int -> Int) -> ST s ()
shiftIdx vmState shifter = ST.modifySTRef (_index vmState) (clamp . shifter)

clamp :: Int -> Int
clamp idx =
  if idx < 0
    then 0
    else
      if idx >= _memorySize
        then _memorySize - 1
        else idx
