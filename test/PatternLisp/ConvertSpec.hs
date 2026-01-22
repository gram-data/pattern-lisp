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
defaultOutputToGram p
  | isSuffixOf ".plisp" p = replaceExtension p "plisp.gram"
  | otherwise               = p ++ ".plisp.gram"

-- | Default output path for --to-plisp: foo.gram or foo.plisp.gram → foo.plisp; else replace or append .plisp.
defaultOutputToPlisp :: FilePath -> FilePath
defaultOutputToPlisp p
  | isSuffixOf ".plisp.gram" p = take (length p - 11) p ++ ".plisp"
  | otherwise                    = replaceExtension p "plisp"

-- | Helper to check if a string is a suffix of another.
isSuffixOf :: String -> String -> Bool
isSuffixOf suffix s = reverse suffix == take (length suffix) (reverse s)

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

  describe "gram→plisp" $ do
    it "valid pattern-lisp gram converts to (begin ...)" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let gram = tmp </> "program.plisp.gram"
          outPlisp = defaultOutputToPlisp gram
      writeFile gram "{ kind: \"Pattern Lisp\" }\n[:Number {value: 42}]\n[:String {value: \"hello\"}]"
      (ec, _stdout, _stderr) <- runPatternLisp ["--to-plisp", gram]
      ec `shouldBe` ExitSuccess
      exists <- doesFileExist outPlisp
      exists `shouldBe` True
      content <- readFile outPlisp
      content `shouldBe` "(begin 42 \"hello\")"
      removePathForcibly tmp

    it "default output foo.plisp.gram → foo.plisp" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let gram = tmp </> "foo.plisp.gram"
          expected = tmp </> "foo.plisp"
      writeFile gram "{ kind: \"Pattern Lisp\" }\n[:Number {value: 1}]"
      (ec, _, _) <- runPatternLisp ["--to-plisp", gram]
      ec `shouldBe` ExitSuccess
      exists <- doesFileExist expected
      exists `shouldBe` True
      removePathForcibly tmp

    it "gram missing kind: \"Pattern Lisp\" yields clear error" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let gram = tmp </> "not-program.gram"
          wouldBeOut = defaultOutputToPlisp gram
      writeFile gram "{ kind: \"Not Pattern Lisp\" }\n[:Number {value: 1}]"
      (ec, _, stderr) <- runPatternLisp ["--to-plisp", gram]
      ec `shouldSatisfy` \c -> c /= ExitSuccess
      stderr `shouldContain` "kind: Pattern Lisp"
      exists <- doesFileExist wouldBeOut
      exists `shouldBe` False
      removePathForcibly tmp

    it "invalid value patterns yield clear error" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let gram = tmp </> "bad-values.gram"
      writeFile gram "{ kind: \"Pattern Lisp\" }\n[:Unknown {what: 1}]"
      (ec, _, _) <- runPatternLisp ["--to-plisp", gram]
      ec `shouldSatisfy` \c -> c /= ExitSuccess
      removePathForcibly tmp

  describe ".plisp.gram convention" $ do
    it "plisp→gram default output is *.plisp.gram" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let plisp = tmp </> "example.plisp"
          expected = tmp </> "example.plisp.gram"
          notExpected = tmp </> "example.gram"
      writeFile plisp "(+ 1 2)"
      (ec, _, _) <- runPatternLisp ["--to-gram", plisp]
      ec `shouldBe` ExitSuccess
      exists <- doesFileExist expected
      exists `shouldBe` True
      notExists <- doesFileExist notExpected
      notExists `shouldBe` False
      removePathForcibly tmp

    it "gram→plisp with *.plisp.gram input uses *.plisp as default output and converts successfully" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let gram = tmp </> "example.plisp.gram"
          expected = tmp </> "example.plisp"
          notExpected = tmp </> "example.plisp.gram.plisp"
      writeFile gram "{ kind: \"Pattern Lisp\" }\n[:Number {value: 42}]\n[:String {value: \"test\"}]"
      (ec, _, _) <- runPatternLisp ["--to-plisp", gram]
      ec `shouldBe` ExitSuccess
      exists <- doesFileExist expected
      exists `shouldBe` True
      notExists <- doesFileExist notExpected
      notExists `shouldBe` False
      content <- readFile expected
      content `shouldBe` "(begin 42 \"test\")"
      removePathForcibly tmp

    it "gram→plisp with *.gram input (not *.plisp.gram) uses *.plisp as default output" $ do
      tmp <- getTemporaryDirectory >>= \d -> createTempDirectory d "convert_"
      let gram = tmp </> "example.gram"
          expected = tmp </> "example.plisp"
      writeFile gram "{ kind: \"Pattern Lisp\" }\n[:Number {value: 100}]"
      (ec, _, _) <- runPatternLisp ["--to-plisp", gram]
      ec `shouldBe` ExitSuccess
      exists <- doesFileExist expected
      exists `shouldBe` True
      content <- readFile expected
      -- Single value is output without (begin ...), multiple values use (begin ...)
      content `shouldBe` "100"
      removePathForcibly tmp
