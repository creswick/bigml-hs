{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Text (Text)

import Test.HUnit (Assertion, assertFailure, assertEqual )
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCase)
import Test.HUnit ( (@=?) )

import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty ( TestTree, defaultIngredients, defaultMainWithIngredients
                  , testGroup )
import Test.Tasty.Ingredients (Ingredient )
import Test.Tasty.Runners.AntXML ( antXMLRunner )

import System.Environment (getEnv)

import BigML
import BigML.Types

main :: IO ()
main = do
  uname <- getEnv "BIGML_USERNAME"
  key <- getEnv "BIGML_API_KEY"
  case uname of
    [] -> fail "You must set the BIGML_USERNAME and BIGML_APIKEY environment variables before running the tests"
    _  -> do
      let mlState = devState { username = uname, api_key = key }
      putStrLn ("Using details: "++ show devState)
      defaultMainWithIngredients ingredients (tests mlState)

ingredients :: [Ingredient]
ingredients = antXMLRunner : defaultIngredients

tests :: BigMLState -> TestTree
tests mlState = testGroup "Tests"
 [ testCase "Create a source from a file" $ test_bigml_action mlState (create_source $ FileSource "test/resources/iris.csv")
 , testCase "Create a dataset from a file" $ test_bigml_action mlState (file_to_dataset "test/resources/iris.csv")
 ]

-- | Run a BigML action with a given state, triggering test failure if the BigML action resulted in a Left.
test_bigml_action :: BigMLState -> BigML (Either String a) -> Assertion
test_bigml_action mlState action = do
  eRes <- runBigML mlState action
  case eRes of
    Left  err -> assertFailure err
    Right res -> return ()

file_to_dataset :: FilePath -> BigML (Either String CreateResponse)
file_to_dataset filepath = do
  eRes <- create_source $ FileSource filepath
  case eRes of
    Left         err -> return $ Left err
    Right sourceResp -> let sResource = resp_resource sourceResp
                        in whenReady sResource (create_dataset (SourceID sResource))
