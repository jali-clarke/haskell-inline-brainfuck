{-# LANGUAGE RankNTypes #-}

module Brainfuck.VM (VMState, runWithVMState, Brainfuck.VM.read, modify, write, shiftIdx) where

import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8)

data VMState s = VMState {_memory :: MV.STVector s Word8, _index :: Int}

_memorySize :: Int
_memorySize = 1024

runWithVMState :: (forall s. VMState s -> ST s a) -> a
runWithVMState withState =
  runST $ do
    memory <- MV.replicate _memorySize 0
    let newState = VMState {_memory = memory, _index = 0}
    withState newState

read :: VMState s -> ST s Word8
read vmState = MV.unsafeRead (_memory vmState) (_index vmState)

modify :: VMState s -> (Word8 -> Word8) -> ST s ()
modify vmState modifier = MV.unsafeModify (_memory vmState) modifier (_index vmState)

write :: VMState s -> Word8 -> ST s ()
write vmState value = MV.unsafeWrite (_memory vmState) (_index vmState) value

shiftIdx :: VMState s -> (Int -> Int) -> VMState s
shiftIdx vmState shifter = vmState {_index = clamp (shifter (_index vmState))}

clamp :: Int -> Int
clamp idx =
  if idx < 0
    then 0
    else if idx >= _memorySize
      then _memorySize - 1
      else idx
