module ParserTests where

import Language.Noc.Parser
import Test.Hspec
import Data.Either

data ParserTest = Success Program | Error ParseError deriving (Show, Eq)

expected :: [(String, Program)]
expected = [ 
            ("test/examples/square.noc", [Declaration {declName = "square", declVal = [[WordAtom "dup",WordAtom "*"]]},Declaration {declName = "main", declVal = [[FloatAtom 4.0,WordAtom "square"]]}]),
            --------------------------------
            ("test/examples/double.noc", [Declaration {declName = "double", declVal = [[WordAtom "dup",WordAtom "+"]]},Declaration {declName = "main", declVal = [[FloatAtom 2.0,WordAtom "double"]]}]) ]


matchEither :: ([ParseError], [Program]) -> ParserTest
matchEither (a, []) = Error $ head a
matchEither ([], b) = Success $ head b

lookupParserTest :: FilePath -> ParserTest
lookupParserTest path = Success v
                            where (Just v) = lookup path expected

--------------------------------------------------------
testParser :: IO ()
testParser = hspec $ do
                describe "Testing Noc parser:" $ do
                    let test1 = "test/examples/square.noc"
                    let test2 = "test/examples/double.noc"

                    -------------------------------------------------------------------------------
                    it ("Testing '"++test1++"'...") $ do
                        parser <- parseNocFile test1
                        (matchEither (partitionEithers [parser])) `shouldBe` (lookupParserTest test1)

                    -------------------------------------------------------------------------------
                    it ("Testing '"++test2++"'...") $ do
                        parser <- parseNocFile test2
                        (matchEither (partitionEithers [parser])) `shouldBe` (lookupParserTest test2)

                    -------------------------------------------------------------------------------
    