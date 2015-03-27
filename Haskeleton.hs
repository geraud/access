-- Usage: runhaskell Haskeleton.hs New.Module
module Haskeleton (main) where

import Data.List (groupBy, isInfixOf)
import Data.Monoid ((<>))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (joinPath, takeDirectory, (<.>))

main :: IO ()
main = do
    ms <- getArgs
    mapM_ createDirectories ms
    mapM_ writeFiles ms
    mapM_ updateFiles ms
  where
    createDirectories m = do
        createBenchmarkDirectory m
        createLibraryDirectory m
        createTestSuiteDirectory m
    writeFiles m = do
        writeBenchmarkFile m
        writeLibraryFile m
        writeTestSuiteFile m
    updateFiles m = do
        updateCabal m
        updateCriterion m
        updateLibrary m

--

createDirectory :: FilePath -> IO ()
createDirectory = createDirectoryIfMissing True . takeDirectory

path :: FilePath -> String -> String -> FilePath
path d s m = joinPath (d : parts (m <> s)) <.> "hs"

parts :: String -> [String]
parts = split '.'

placeholder :: String
placeholder = "New.Module"

pragma :: String
pragma = "-- HASKELETON: "

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace xs@(h : t) old new =
    if a == old
        then new <> replace b old new
        else h : replace t old new
  where
    (a, b) = splitAt (length old) xs

split :: (Eq a) => a -> [a] -> [[a]]
split x xs = fmap (dropWhile (== x)) (groupBy (const (x /=)) xs)

updateFile :: FilePath -> String -> IO ()
updateFile p m = do
    contents <- readFile p
    seq (length contents) (return ())
    writeFile p (unlines (go =<< lines contents))
  where
    go line = if pragma `isInfixOf` line
        then [replace (replace line pragma "") placeholder m, line]
        else [line]

-- Benchmark

createBenchmarkDirectory :: String -> IO ()
createBenchmarkDirectory = createDirectory . benchmarkPath

benchmarkDirectory :: String
benchmarkDirectory = "benchmark"

benchmarkPath :: String -> FilePath
benchmarkPath = path benchmarkDirectory benchmarkSuffix

benchmarkTemplate :: String -> String
benchmarkTemplate m = unlines
    [ "module " <> m <> "Bench (benchmarks) where"
    , ""
    , "import " <> m <> " ()"
    , ""
    , "import Criterion"
    , ""
    , "benchmarks :: [Benchmark]"
    , "benchmarks = []"
    ]

benchmarkSuffix :: String
benchmarkSuffix = "Bench"

writeBenchmarkFile :: String -> IO ()
writeBenchmarkFile m = writeFile (benchmarkPath m) (benchmarkTemplate m)

-- Cabal

cabalPath :: FilePath
cabalPath = "access.cabal"

updateCabal :: String -> IO ()
updateCabal = updateFile cabalPath

-- Criterion

criterionPath :: FilePath
criterionPath = benchmarkPath ""

updateCriterion :: String -> IO ()
updateCriterion = updateFile criterionPath

-- Library

createLibraryDirectory :: String -> IO ()
createLibraryDirectory = createDirectory . libraryPath

libraryDirectory :: String
libraryDirectory = "library"

libraryPath :: String -> FilePath
libraryPath = path libraryDirectory librarySuffix

libraryTemplate :: String -> String
libraryTemplate m = unlines
    [ "-- | TODO"
    , "module " <> m <> " () where"
    ]

librarySuffix :: String
librarySuffix = ""

updateLibrary :: String -> IO ()
updateLibrary = updateFile (libraryPath "Access")

writeLibraryFile :: String -> IO ()
writeLibraryFile m = writeFile (libraryPath m) (libraryTemplate m)

-- Test Suite

createTestSuiteDirectory :: String -> IO ()
createTestSuiteDirectory = createDirectory . testSuitePath

testSuiteDirectory :: String
testSuiteDirectory = "test-suite"

testSuitePath :: String -> FilePath
testSuitePath = path testSuiteDirectory testSuiteSuffix

testSuiteTemplate :: String -> String
testSuiteTemplate m = unlines
    [ "module " <> m <> "Spec (spec) where"
    , ""
    , "import " <> m <> " ()"
    , ""
    , "import Test.Hspec"
    , ""
    , "spec :: Spec"
    , "spec = it \"is\" pending"
    ]

testSuiteSuffix :: String
testSuiteSuffix = "Spec"

writeTestSuiteFile :: String -> IO ()
writeTestSuiteFile m = writeFile (testSuitePath m) (testSuiteTemplate m)
