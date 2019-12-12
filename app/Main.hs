module Main where
import Emulator

main :: IO ()
main = do emulator <- createEmulator
          runEmulator emulator
          return ()
