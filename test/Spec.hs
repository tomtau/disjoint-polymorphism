
import Control.Monad (forM_)
import Test.Tasty
import Test.Tasty.Hspec
import System.FilePath
import Data.List (isSuffixOf)
import System.Directory
import Text.PrettyPrint.ANSI.Leijen (Doc)
import SEDEL

data TestResult = Res Doc Bool

instance Eq TestResult where
  (Res _ b) == (Res _ b') = b && b'

instance Show TestResult where
  show (Res i _) = show i

testPath :: FilePath
testPath = "examples"


discoverTestCases :: FilePath -> IO [(String, FilePath)]
discoverTestCases directory =
  do fileNames <- filter (isSuffixOf ".txt") <$>
                  getDirectoryContents directory
     return (map (\f -> (dropExtension f, f)) fileNames)

spec :: Spec
spec = do
  cases <- runIO (discoverTestCases testPath)
  curr <- runIO getCurrentDirectory
  runIO (setCurrentDirectory $ curr </> testPath)
  describe ("Test suite [" ++ testPath ++ "]") $
    forM_ cases $ \(name, filePath) -> do
      ((d, md), ok) <- runIO $ evalFile filePath
      it ("Testing " ++ name) $
        let msg = Res d True in
        if ok then msg `shouldBe` msg
        else case md of
        Nothing -> expectationFailure (show msg)
        Just d' -> msg `shouldBe` Res d' False
  runIO (setCurrentDirectory curr)



main :: IO ()
main = do
  fileTests <- testSpec "Unit tests" spec
  let tests = testGroup "Fun tests" [fileTests]
  defaultMain tests
