module Main.Simulate(simulate) where
import Text.Printf

getSimulation :: Int -> Maybe (IO ())
getSimulation n
    -- | n == 237 = Just runSimulation237
    | otherwise = Nothing

simulate :: Int -> IO ()
simulate problemNo = case getSimulation problemNo of
    Nothing -> printf "Could not find simulation associated with problem %d\n" problemNo
    Just simulation -> printf "Running simulation associated with problem %d\n" problemNo >> simulation