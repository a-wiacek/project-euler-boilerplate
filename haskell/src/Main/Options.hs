module Main.Options(parseOptions, Option(..)) where
import Options.Applicative

data Option = Tests | Run Int | Generate Int | Delete Int | Simulate Int

optionTests :: Parser Option
optionTests = flag' Tests (long "tests" <> short 't' <> help "Generate tests for all solutions")

optionRun :: Parser Option
optionRun = Run <$> option auto
     ( long "run"
    <> short 'r'
    <> metavar "PROBLEMNO"
    <> help "Choose number of problem to run" )

optionGenerate :: Parser Option
optionGenerate = Generate <$> option auto
     ( long "generate"
    <> short 'g'
    <> metavar "PROBLEMNO"
    <> help "Generate template of solution: add entry to Main/Run/Eulers.hs and new file" )

optionDelete :: Parser Option
optionDelete = Delete <$> option auto
     ( long "delete"
    <> short 'd'
    <> metavar "PROBLEMNO"
    <> help "Delete solution file and entry in Main/Run/Eulers.hs" )

optionSimulate :: Parser Option
optionSimulate = Simulate <$> option auto
     ( long "simulate"
    <> short 's'
    <> metavar "PROBLEMNO"
    <> help "Run simulation associated with problem" )

options :: Parser Option
options = optionTests <|> optionRun <|> optionGenerate <|> optionDelete <|> optionSimulate

parseOptions :: IO Option
parseOptions = execParser $ info (options <**> helper) (header "Project Euler solutions manager")