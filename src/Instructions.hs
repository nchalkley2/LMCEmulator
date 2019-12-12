{-# LANGUAGE GADTs #-}
module Instructions where
import Types
import LMC
import Control.Lens
import Control.Lens.Getter
import Control.Lens.Setter
import Data.Type.Equality
import qualified Data.Vector.Unboxed.Mutable as MV

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