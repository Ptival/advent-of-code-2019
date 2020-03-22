-- |

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Intcode (
  findInputsForTarget,
  initialMemoryFromString,
  initialProgramState,
  memory,
  programMemory,
  restore1202Program,
  runProgram,
  stepInstruction,
  ) where

import qualified Control.Lens            as L
import           Control.Monad.Loops     ( untilJust, whileM_ )
import           Data.String.Interpolate
import           Polysemy                ( Member, Sem )
import           Polysemy.Error          ( Error, throw )
import           Polysemy.Trace          ( Trace, trace )
import           Polysemy.State          ( State, get, gets, modify', put )

data Opcode
  = Halt
  | Addition
  | Multiplication

opcode :: Int -> Opcode
opcode 1  = Addition
opcode 2  = Multiplication
opcode 99 = Halt
opcode o  = error [i|Unrecognized opcode: #{o}|]

newtype Memory = Memory { _memory :: [Int] }
  deriving ( Show )

L.makeLenses ''Memory

data ProgramState = ProgramState
  { _programCounter :: Int
  , _programHalted  :: Bool
  , _programMemory  :: Memory
  }
  deriving ( Show )

L.makeLenses ''ProgramState

-- | Bumps the program counter, and returns the previous value of the program
-- counter
bumpProgramCounter ::
  Member (State ProgramState) e =>
  Sem e Int
bumpProgramCounter =
  do c <- gets (L.view programCounter)
     modify' (L.over programCounter (+ 1))
     return c

getMemoryAddress :: Int -> L.Getter ProgramState (Maybe Int)
getMemoryAddress a = L.pre (programMemory . memory . L.element a)

setMemoryAddress :: Int -> L.Setter' ProgramState Int
setMemoryAddress a = programMemory . memory . L.element a

data ProgramError
  = OutOfBoundAccess
  deriving ( Show )

memoryRead ::
  Member (Error ProgramError) e =>
  Member (State ProgramState) e =>
  Member Trace                e =>
  Int -> -- ^ address to read from
  Sem e Int
memoryRead a =
  gets (L.view (getMemoryAddress a)) >>= \case
  Nothing -> throw OutOfBoundAccess
  Just v  ->
    do trace [i|Read value #{v} at address #{a}|]
       return v

memoryWrite ::
  Member (State ProgramState) e =>
  Member Trace                e =>
  Int -> -- ^ address to write at
  Int -> -- ^ value to write
  Sem e ()
memoryWrite a v =
  do trace [i|Writing value #{v} at address #{a}|]
     modify' (L.set (setMemoryAddress a) v)

readBumpProgramCounter ::
  Member (Error ProgramError) e =>
  Member (State ProgramState) e =>
  Member Trace                e =>
  Sem e Int
readBumpProgramCounter = bumpProgramCounter >>= memoryRead

binaryOperation ::
  Member (Error ProgramError) e =>
  Member (State ProgramState) e =>
  Member Trace                e =>
  (Int -> Int -> Int) ->
  Sem e ()
binaryOperation op =
  do arg1         <- readBumpProgramCounter >>= memoryRead
     arg2         <- readBumpProgramCounter >>= memoryRead
     writeAddress <- readBumpProgramCounter
     let result = op arg1 arg2
     memoryWrite writeAddress result

stepInstruction ::
  Member (Error ProgramError) e =>
  Member Trace                e =>
  Member (State ProgramState) e =>
  Sem e ()
stepInstruction =
  do instruction <- readBumpProgramCounter
     case opcode instruction of
       Addition       -> trace "Addition"       >> binaryOperation (+)
       Multiplication -> trace "Multiplication" >> binaryOperation (*)
       Halt           -> trace "Halt"           >> modify' (L.set programHalted True)

dumpMemory ::
  Member Trace                e =>
  Member (State ProgramState) e =>
  Sem e ()
dumpMemory = gets (L.view programMemory) >>= trace . show

runProgram ::
  Member (Error ProgramError) e =>
  Member Trace                e =>
  Member (State ProgramState) e =>
  Sem e ()
runProgram =
  whileM_
  (not <$> gets (L.view programHalted))
  (trace "stepInstruction" >> stepInstruction >> dumpMemory)

initialMemoryFromString :: String -> Memory
initialMemoryFromString s = Memory $ read [i|[#{s}]|]

initialProgramState :: Memory -> ProgramState
initialProgramState m = ProgramState
  { _programCounter = 0
  , _programHalted  = False
  , _programMemory  = m
  }

restore1202Program ::
  Member (Error ProgramError) e =>
  Member Trace                e =>
  Member (State ProgramState) e =>
  Sem e ()
restore1202Program =
  do memoryWrite 1 12
     memoryWrite 2 2

resetProgram ::
  Member (State ProgramState) e =>
  Member Trace                e =>
  Memory ->
  Sem e ()
resetProgram m = put (initialProgramState m)

setNoun ::
  Member (Error ProgramError) e =>
  Member (State ProgramState) e =>
  Member Trace                e =>
  Int ->
  Sem e ()
setNoun = memoryWrite 1

setVerb ::
  Member (Error ProgramError) e =>
  Member (State ProgramState) e =>
  Member Trace                e =>
  Int ->
  Sem e ()
setVerb = memoryWrite 2

getOutput ::
  Member (Error ProgramError) e =>
  Member (State ProgramState) e =>
  Member Trace                e =>
  Sem e Int
getOutput = memoryRead 0

nextNounVerb :: (Int, Int) -> (Int, Int)
nextNounVerb (a, b) | a >= 99 && b >= 99 = error "nextNounVerb: This was guaranteed not to happen"
                    | otherwise          = if b == 99
                                           then (a + 1, 0)
                                           else (a, b + 1)

findInputsForTarget ::
  Member (Error ProgramError) e =>
  Member (State (Int, Int))   e =>
  Member (State ProgramState) e =>
  Member Trace                e =>
  Memory -> -- ^ initial memory
  Int ->    -- ^ target
  Sem e (Int, Int)
findInputsForTarget initialMemory target =
  untilJust $
  do resetProgram initialMemory
     restore1202Program
     nv@(noun, verb) <- get
     put (nextNounVerb nv) -- before we forget
     trace [i|Testing input #{nv}|]
     setNoun noun
     setVerb verb
     runProgram
     output <- getOutput
     if output == target
       then return (Just nv)
       else do trace "Failed, NEXT"
               return Nothing
