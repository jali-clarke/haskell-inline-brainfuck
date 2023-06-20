{-# LANGUAGE DeriveDataTypeable #-}

module Brainfuck.Program (Program (..), interpret) where

import Control.Monad.ST (ST)
import Data.Data (Data)
import Data.Word (Word8)

import qualified Brainfuck.VM as VM

data Program =
  Inc Program
  | Dec Program
  | Lft Program
  | Rgt Program
  | Inp Program
  | Out Program
  | Loop Program Program
  | Done
  deriving Data

interpret :: Program -> [Word8] -> [Word8]
interpret program input = VM.runWithVMState (interpret' program input)

interpret' :: Program -> [Word8] -> VM.VMState s -> ST s [Word8]
interpret' program input vmState =
  case program of
    Inc program' -> VM.modify vmState (+ 1) *> interpret' program' input vmState
    Dec program' -> VM.modify vmState (subtract 1) *> interpret' program' input vmState
    Lft program' -> interpret' program' input (VM.shiftIdx vmState (subtract 1))
    Rgt program' -> interpret' program' input (VM.shiftIdx vmState (+ 1))
    Inp program' ->
      case input of
        [] -> error "Brainfuck.Program.interpret': input is empty"
        value : input' -> VM.write vmState value *> interpret' program' input' vmState
    Out program' -> do
      value <- VM.read vmState
      fmap (value :) (interpret' program' input vmState)
    Loop loopBody program' -> do
      value <- VM.read vmState
      if value == 0
        then interpret' program' input vmState
        else interpret' loopBody input vmState *> interpret' program input vmState
    Done -> pure []
