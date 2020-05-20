{-# LANGUAGE LambdaCase #-}

module Euler(runEuler) where

import Main.Generate
import Main.Options
import Main.Simulate
import Main.Tests
import Main.Run

runEuler :: IO ()
runEuler = parseOptions >>= \case
    Tests -> generateTests
    Run n -> run n
    Generate n -> generate n
    Delete n -> ungenerate n
    Simulate n -> simulate n
