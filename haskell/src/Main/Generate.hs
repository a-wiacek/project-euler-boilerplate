module Main.Generate(generate, ungenerate) where
import Data.List(delete)
import Text.Printf
import System.Directory
import System.FilePath
import System.Exit

eulersPath :: FilePath
eulersPath = joinPath ["src", "Main", "Run", "Eulers.hs"]

solutionPath :: Int -> FilePath
solutionPath problemNo = joinPath ["src", "Solutions", printf "Euler%03d.hs" problemNo]

thisImport :: Int -> String
thisImport problemNo = printf "import Solutions.Euler%03d(euler%03d)" problemNo problemNo

thisEntry :: Int -> String
thisEntry problemNo = printf "    , (%d, euler%03d)" problemNo problemNo

readEulersFile :: IO ([String], [String], [String], [String], [String])
readEulersFile = do
    fileContent <- readFile eulersPath
    return $! length fileContent
    let (header, fileContent') = splitAt 3 $ lines fileContent
    let (imports, fileContent'') = break null fileContent'
    let (eulersDecl, fileContent''') = splitAt 3 fileContent''
    let (eulerEntries, getEulerDecl) = break (=="    ]") fileContent'''
    return (header, imports, eulersDecl, eulerEntries, getEulerDecl)

writeEulersFile :: String -> IO ()
writeEulersFile = writeFile eulersPath

insertImport :: Int -> [String] -> [String]
insertImport problemNo imports =
    -- line template: import Solutions.EulerXYZ(eulerXYZ)
    let prefixLen = length "import Solutions.Euler"
        isLowerImport i = read (take 3 $ drop prefixLen i) < problemNo
        (lowerImports, higherImports) = span isLowerImport imports
    in lowerImports ++ thisImport problemNo : higherImports

deleteImport :: Int -> [String] -> [String]
deleteImport problemNo = delete $ thisImport problemNo

insertEntry :: Int -> [String] -> [String]
insertEntry problemNo entries =
    let prefixLen = length "    [ ("
        isLowerEntry e = read (takeWhile (/=',') $ drop prefixLen e) < problemNo
        (lowerEntries, higherEntries) = span isLowerEntry entries
    in lowerEntries ++ thisEntry problemNo : higherEntries

deleteEntry :: Int -> [String] -> [String]
deleteEntry problemNo = delete $ thisEntry problemNo
    
generateEulersEntry :: Int -> IO ()
generateEulersEntry problemNo = do
    (header, imports, eulersDecl, eulerEntries, getEulerDecl) <- readEulersFile
    let newFileContent = unlines $ concat
                         [ header
                         , insertImport problemNo imports
                         , eulersDecl
                         , insertEntry problemNo eulerEntries
                         , getEulerDecl ]
    writeEulersFile newFileContent

deleteEulersEntry :: Int -> IO ()
deleteEulersEntry problemNo = do
    (header, imports, eulersDecl, eulerEntries, getEulerDecl) <- readEulersFile
    let newFileContent = unlines $ concat
                        [ header
                        , deleteImport problemNo imports
                        , eulersDecl
                        , deleteEntry problemNo eulerEntries
                        , getEulerDecl ]
    writeEulersFile newFileContent

addEulerFile :: Int -> IO ()
addEulerFile problemNo = writeFile (solutionPath problemNo) $ printf
    "module Solutions.Euler%03d where\n\
    \\n\
    \euler%03d :: IO String\n\
    \euler%03d = return undefined"
    problemNo problemNo problemNo

removeEulerFile :: Int -> IO ()
removeEulerFile = removeFile . solutionPath

generate :: Int -> IO ()
generate problemNo = if problemNo > 0 && problemNo < 1000
    then doesFileExist (solutionPath problemNo) >>= \fileExists -> if fileExists
        then printf "File Euler%03d.hs already exists, aborting" problemNo >> exitFailure
        else do generateEulersEntry problemNo
                addEulerFile problemNo
                printf "Successfully generated Euler%03d.hs and corresponding entries" problemNo
    else putStrLn "Number must be between 1 and 999" >> exitFailure

ungenerate :: Int -> IO ()
ungenerate problemNo = do
    deleteEulersEntry problemNo
    removeEulerFile problemNo
    printf "Successfully deleted Euler%03d.hs and corresponding entries" problemNo