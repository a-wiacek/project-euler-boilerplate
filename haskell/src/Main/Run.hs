module Main.Run(run) where
import Data.Time
import Main.Run.Eulers(getEuler)
import Text.Printf

run :: Int -> IO ()
run problemNo = case getEuler problemNo of
    Nothing -> printf "Could not find solution for problem %d\n" problemNo
    Just euler -> do 
        printf "Running problem %d\n" problemNo
        start <- getCurrentTime
        output <- euler
        printf "Output: %s\n" output
        end <- getCurrentTime
        printf "Execution time: %s\n" $ show $ diffUTCTime end start