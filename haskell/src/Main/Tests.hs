module Main.Tests(generateTests) where
import Text.Printf
import System.Directory
import System.FilePath
import System.Exit

createSpecFile :: Int -> String -> IO ()
createSpecFile problemNo answer = writeFile filepath fileContent where
    filepath = joinPath ["test", "Solutions", printf "Euler%03dSpec.hs" problemNo] 
    fileContent = printf
        "module Solutions.Euler%03dSpec where\n\
        \import Test.Hspec\n\
        \import Solutions.Euler%03d(euler%03d)\n\
        \\n\
        \spec :: Spec\n\
        \spec = describe \"Problem %d\" $ it \"Returns proper answer\" $ euler%03d `shouldReturn` \"%s\""
        problemNo problemNo problemNo problemNo problemNo answer
    

generateTests :: IO ()
generateTests =
    let filepath = joinPath ["..", "txt", "answers.txt"]
        dirpath = joinPath ["test", "Solutions"]
    in doesFileExist filepath >>= \p -> if not p
        then putStrLn "File answers.txt not found in txt directory" >> exitFailure
        else do removeDirectoryRecursive dirpath
                createDirectoryIfMissing True dirpath
                filter (not . null . snd) . zip [1..] . lines <$> readFile filepath >>= 
                    mapM_ (uncurry createSpecFile)
                putStrLn "Tests generated"
                putStrLn "Warning: tests may take eternity!"