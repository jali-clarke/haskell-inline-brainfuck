{-# LANGUAGE DeriveDataTypeable #-}

module Brainfuck.Program (Program (..), interpret) where

import qualified Brainfuck.VM as VM
import Control.Monad.ST (ST)
import Data.Data (Data)
import Data.Word (Word8)

data Program
  = Inc Program
  | Dec Program
  | Lft Program
  | Rgt Program
  | Inp Program
  | Out Program
  | Loop Program Program
  | Done
  deriving (Data)

interpret :: Program -> [Word8] -> [Word8]
interpret program input = snd $ VM.runWithVMState (interpret' program input)

interpret' :: Program -> [Word8] -> VM.VMState s -> ST s ([Word8], [Word8])
interpret' program input vmState =
  case program of
    Inc program' -> VM.modify vmState (+ 1) *> interpret' program' input vmState
    Dec program' -> VM.modify vmState (subtract 1) *> interpret' program' input vmState
    Lft program' -> VM.shiftIdx vmState (subtract 1) *> interpret' program' input vmState
    Rgt program' -> VM.shiftIdx vmState (+ 1) *> interpret' program' input vmState
    Inp program' ->
      case input of
        [] -> error "Brainfuck.Program.interpret': input is empty"
        value : input' -> VM.write vmState value *> interpret' program' input' vmState
    Out program' -> do
      value <- VM.read vmState
      (unconsumedInput, output) <- interpret' program' input vmState
      pure (unconsumedInput, value : output)
    Loop loopBody program' -> do
      value <- VM.read vmState
      if value == 0
        then interpret' program' input vmState
        else do
          (unconsumedInputFromLoop, loopBodyOutput) <- interpret' loopBody input vmState
          (unconsumedInputFromRest, programOutput) <- interpret' program unconsumedInputFromLoop vmState
          pure (unconsumedInputFromRest, loopBodyOutput ++ programOutput)
    Done -> pure (input, [])
