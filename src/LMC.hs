{-# LANGUAGE ExistentialQuantification, GADTs, ScopedTypeVariables, TemplateHaskell #-}
module LMC where
import Data.Int
import Types
import Control.Monad.Primitive
import qualified Control.Lens as CL
import Control.Lens.Getter
import Control.Lens.Setter
import qualified Data.Vector.Unboxed.Mutable as MV

data Label = Label
  { _name   :: String
  , _index  :: Index
  } deriving (Show, Eq)

$(CL.makeLenses ''Label)

data LMCState a = LMCState
  { _register       :: Int16
  , _programCounter :: Index
  , _instructions   :: [a]
  , _labels         :: [Label]
  } deriving (Show)

$(CL.makeLenses ''LMCState)

incrementPC :: LMCState a -> LMCState a
incrementPC s = let pc = view programCounter s
                in  s {_programCounter = pc + 1}

type LMCMemory = MV.MVector (PrimState IO) Int16

type EmulatorState = (LMCMemory, LMCState AnyInstruction)

class (Show a) => Instruction a where
  exec :: a -> EmulatorState -> IO EmulatorState
  isHalting :: a -> Bool
  isHalting _ = False

data AnyInstruction = forall a . Instruction a => AI a

instance Show AnyInstruction where
  show (AI a) = show a

readMem memory addr = MV.unsafeRead memory $ fromIntegral addr
writeMem memory addr val = MV.unsafeWrite memory (fromIntegral addr) val

data HLT = HLT deriving (Show)
instance Instruction HLT where
  exec _ state = do putStrLn $ 
                      "HALT: Register value: " ++ (show $ view register (snd state))
                    return state
  isHalting _  = True

data ADD = ADD Address deriving (Show)
instance Instruction ADD where
  exec (ADD addr) (memory, state) = do let reg = view register state
                                       x <- readMem memory addr
                                       return
                                        (memory, set register (reg + x) state)

data SUB = SUB Address deriving (Show)
instance Instruction SUB where
  exec (SUB addr) (memory, state) = do let reg = view register state
                                       x <- readMem memory addr
                                       return
                                        (memory, set register (reg - x) state)

data MUL = MUL Address deriving (Show)
instance Instruction MUL where
  exec (MUL addr) (memory, state) = do let reg = view register state
                                       x <- readMem memory addr
                                       return
                                        (memory, set register (reg * x) state)

data DIV = DIV Address deriving (Show)
instance Instruction DIV where
  exec (DIV addr) (memory, state) = do let reg = view register state
                                       x <- readMem memory addr
                                       return
                                        (memory, set register (reg `div` x) state)

data REM = REM Address deriving (Show)
instance Instruction REM where
  exec (REM addr) (memory, state) = do let reg = view register state
                                       x <- readMem memory addr
                                       return
                                        (memory, set register (reg `mod` x) state)

data LDA = LDA Address deriving (Show)
instance Instruction LDA where
  exec (LDA addr) (memory, state) = do x <- readMem memory addr
                                       return
                                        (memory, set register x state)

data STA = STA Address deriving (Show)
instance Instruction STA where
  exec (STA addr) (memory, state) = do writeMem memory addr (view register state)
                                       return (memory, state)

data LIT = LIT Int16 deriving (Show)
instance Instruction LIT where
  exec (LIT val) (memory, state) = return (memory, set register val state)

--   | SHL Address -- Shift register left
--   | SHR Address -- Shift register right
--   | LDA Address -- Load address from memory to register
--   | STA Address -- Store from register to memory at address
--   | BRA Index   -- Unconditional branch to instruction index
--   | BRZ Index   -- Branch if register is 0 to instruction index
--   | BRP Index   -- Branch if register is positive to instruction index
--   | INP         -- Read input from user to register
--   | OUT         -- Print register to screen
--   | LIT Int16   -- Set the register to the value given by argument
--     deriving (Show, Eq, Read)-- data Instruction = 
--     HLT         -- Halt the emulator and return register value
--   | ADD Address -- Add value from memory to register. 
--                 -- Example : ADD 0x000 = register = register + 0x000 
--   | SUB Address -- Subtract value from memory to register. 
--                 -- Example : SUB 0x000 = register = register - 0x000 
--   | MUL Address -- Multiply value from memory with register. 
--                 -- Example : MUL 0x000 = register = register * 0x000
--   | REM Address 
--   | DIV Address 
--   | SHL Address -- Shift register left
--   | SHR Address -- Shift register right
--   | LDA Address -- Load address from memory to register
--   | STA Address -- Store from register to memory at address
--   | BRA Index   -- Unconditional branch to instruction index
--   | BRZ Index   -- Branch if register is 0 to instruction index
--   | BRP Index   -- Branch if register is positive to instruction index
--   | INP         -- Read input from user to register
--   | OUT         -- Print register to screen
--   | LIT Int16   -- Set the register to the value given by argument