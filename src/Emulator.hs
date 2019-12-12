{-# LANGUAGE ExistentialQuantification, TemplateHaskell, LiberalTypeSynonyms, InstanceSigs #-}
module Emulator (createEmulator, runEmulator) where
import LMC
import Instructions
import Data.Int
import Data.String
import Data.Strings
import Control.Lens.Getter
import Control.Lens.Setter
import qualified Data.Vector.Unboxed.Mutable as MV

import Debug.Trace

removeComments str = fst (strSplit "//" str)

-- compileEmulator :: String -> LMCState AnyInstruction
-- compileEmulator file = let fLines = fmap removeComments $ lines file
--                            syntax = fmap words fLines
--                        in fmap (\line -> if (length line) > 0
--                                          then 
--                        LMCState 0 0 [] [] --fmap removeComments fLines

createEmulatorFromStr :: String -> IO EmulatorState
createEmulatorFromStr str = do memory <- MV.replicate 0xFFFF (0 :: Int16)
                               return (memory, compileEmulator str)

createEmulator :: IO EmulatorState
createEmulator = do memory <- MV.replicate 0xFFFF (0 :: Int16)
                    return (memory, LMCState 0 0 [] [])

runEmulator :: EmulatorState -> IO EmulatorState
runEmulator state = do let index = fromIntegral (view programCounter $ snd state)
                       let instructionCount = (length $ view instructions $ snd state)
                       if index >= instructionCount
                       then do putStrLn "Emulator finished"
                               return state
                       else (\(AI i) -> do putStrLn $ (show index) ++ ": " ++ (show i)
                                           (newMem, newState) <- exec i state
                                           if isHalting i
                                           then return (newMem, newState)
                                           else runEmulator (newMem, incrementPC newState)
                            ) $ (view instructions $ snd state) !! index
