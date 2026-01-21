module PatternLisp.ConvertSpec (spec) where

import Test.Hspec
import System.Process (readProcessWithExitCode)
import System.Directory (doesFileExist, removePathForcibly, getTemporaryDirectory)
import System.IO.Temp (createTempDirectory)
import System.FilePath ((</>), replaceExtension)
import System.Exit (ExitCode(..))

-- | Run pattern-lisp with the given arguments. Uses `cabal run pattern-lisp --`
-- so the exe is built and run. Returns (exitcode, stdout, stderr).
-- The working directory is the current directory (project root when run via cabal test).
runPatternLisp :: [String] -> IO (ExitCode, String, String)
runPatternLisp args = readProcessWithExitCode "cabal" (["run", "pattern-lisp", "--"] ++ args) ""

-- | Default output path for --to-gram: foo.plisp → foo.plisp.gram; else append .plisp.gram.
defaultOutputToGram :: FilePath -> FilePath
defaultOutputToGram p = replaceExtension p "plisp.gram"

spec :: Spec
spec = describe "Convert (plisp↔gram)" $ do
  describe "plisp→gram" $ do
    it "valid .plisp converts and writes gram" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let plisp = tmp </> "script.plisp"
          outGram = defaultOutputToGram plisp
      writeFile plisp "(+ 1 2)"
      (ec, _stdout, _stderr) <- runPatternLisp ["--to-gram", plisp]
      ec `shouldBe` ExitSuccess
      exists <- doesFileExist outGram
      exists `shouldBe` True
      content <- readFile outGram
      content `shouldContain` "kind"
      content `shouldContain` "Pattern Lisp"
      removePathForcibly tmp

    it "default output path foo.plisp → foo.plisp.gram" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let plisp = tmp </> "foo.plisp"
          expected = tmp </> "foo.plisp.gram"
      writeFile plisp "42"
      (ec, _, _) <- runPatternLisp ["--to-gram", plisp]
      ec `shouldBe` ExitSuccess
      exists <- doesFileExist expected
      exists `shouldBe` True
      removePathForcibly tmp

    it "invalid plisp reports error and no output" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let plisp = tmp </> "bad.plisp"
          wouldBeOut = defaultOutputToGram plisp
      writeFile plisp "(+ 1 2"  -- parse error
      (ec, _, _) <- runPatternLisp ["--to-gram", plisp]
      ec `shouldSatisfy` \c -> c /= ExitSuccess
      exists <- doesFileExist wouldBeOut
      exists `shouldBe` False
      removePathForcibly tmp

    it "explicit -o is used" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let plisp = tmp </> "x.plisp"
          custom = tmp </> "custom.gram"
      writeFile plisp "true"
      (ec, _, _) <- runPatternLisp ["--to-gram", plisp, "-o", custom]
      ec `shouldBe` ExitSuccess
      exists <- doesFileExist custom
      exists `shouldBe` True
      removePathForcibly tmp
