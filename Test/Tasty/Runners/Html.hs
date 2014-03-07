{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run a 'Tasty.TestTree' and produce an HTML file summarising the test results.
module Test.Tasty.Runners.Html (htmlRunner) where

import Control.Applicative
import Data.List (intersperse)
import Control.Monad ((>=>))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..), Sum(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup.Applicative (Traversal(..))
import Data.Tagged (Tagged(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (memptydefault, mappenddefault)

import qualified Data.ByteString as B
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.State as State
import qualified Data.Functor.Compose as Functor
import qualified Data.IntMap as IntMap
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.Runners as Tasty
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.String (renderHtml)

import Paths_tasty_html (getDataFileName)

--------------------------------------------------------------------------------
newtype HtmlPath = HtmlPath FilePath
  deriving (Typeable)

instance Tasty.IsOption (Maybe HtmlPath) where
  defaultValue = Nothing
  parseValue = Just . Just . HtmlPath
  optionName = Tagged "html"
  optionHelp = Tagged "A file path to store the test results in Html"


--------------------------------------------------------------------------------
data Summary = Summary { summaryFailures :: Sum Int
                       , summarySuccesses :: Sum Int
                       , htmlRenderer :: H.Markup
                       } deriving (Generic)

instance Monoid Summary where
  mempty = memptydefault
  mappend = mappenddefault


--------------------------------------------------------------------------------
{-|

  To run tests using this ingredient, use 'Tasty.defaultMainWithIngredients',
  passing 'htmlRunner' as one possible ingredient. This ingredient will run
  tests if you pass the @--html@ command line option. For example,
  @--html=results.html@ will run all the tests and generate @results.htmll@ as output.

-}
htmlRunner :: Tasty.Ingredient
htmlRunner = Tasty.TestReporter optionDescription runner
 where
  optionDescription = [ Tasty.Option (Proxy :: Proxy (Maybe HtmlPath)) ]
  runner options testTree = do
    HtmlPath path <- Tasty.lookupOption options

    return $ \statusMap ->

      let
        runTest _ testName _ = Traversal $ Functor.Compose $ do
          i <- State.get

          summary <- lift $ STM.atomically $ do
            status <- STM.readTVar $
              fromMaybe (error "Attempted to lookup test by index outside bounds") $
              IntMap.lookup i statusMap

            let splitBr = H.toMarkup . intersperse H.br . map H.toMarkup . lines
                mkSummary contents = mempty { htmlRenderer = item contents }

                mkSuccess desc = (mkSummary $ do
                    H.span ! HA.class_ "badge badge-success" $ H.toMarkup testName
                    H.br
                    H.small $ H.toMarkup desc) { summarySuccesses = Sum 1 }

                mkFailure reason = (mkSummary $ do
                    H.span ! HA.class_ "badge badge-important" $ H.toMarkup testName
                    H.br
                    H.small $ splitBr reason) { summaryFailures = Sum 1 }

            case status of
              -- If the test is done, generate HTML for it
              Tasty.Done result
                | Tasty.resultSuccessful result -> pure $
                    mkSuccess $ Tasty.resultDescription result
                | otherwise -> pure $ mkFailure $ Tasty.resultDescription result
              -- Otherwise the test has either not been started or is currently
              -- executing
              _ -> STM.retry

          Const summary <$ State.modify (+1)

        runGroup groupName children = Traversal $ Functor.Compose $ do
          Const soFar <- Functor.getCompose $ getTraversal children
          let grouped = item
                      $ do H.span $ H.toMarkup groupName
                           tree $ htmlRenderer soFar

          pure $ Const
            soFar { htmlRenderer = grouped }

      in do
        (Const summary, tests) <-
          flip State.runStateT 0 $ Functor.getCompose $ getTraversal $
          Tasty.foldTestTree
            Tasty.trivialFold { Tasty.foldSingle = runTest
                              , Tasty.foldGroup = runGroup
                              }
            options
            testTree

        css <- includeMarkup "/data/bootstrap-combined.min.css"
        style <- includeMarkup "/data/style.css"
        js  <- includeMarkup "/data/jquery-2.1.0.min.js"

        writeFile path $
          renderHtml $
            H.docTypeHtml $ do
              H.head $ do H.title "Test Results"
                          H.style css
                          H.style style
                          H.script js
              H.body $ do
                H.div ! HA.class_ "row" $
                  H.div ! HA.class_ "status_area span12" $ do
                    H.h4 ! HA.class_ "header" $ "Status"
                    H.table ! HA.id "summary" $
                      H.tr $ do
                        H.td ! HA.class_ "status" $ H.span "Successes"
                        H.td ! HA.class_ "number" $ H.toMarkup . getSum
                                                  $ summarySuccesses summary
                        H.tr $ do
                          H.td ! HA.class_ "status" $ H.span "Failures"
                          H.td ! HA.class_ "number" $ H.toMarkup . getSum
                                                    $ summaryFailures summary
                          H.tr $ do
                            H.td ! HA.class_ "status" $ H.span "Total"
                            H.td ! HA.id  "total" $ H.toMarkup tests
                H.div ! HA.class_ "row" $
                  H.div ! HA.class_ "results_area span12" $ do
                      H.h4 ! HA.class_ "header" $ "Results"
                      H.div ! HA.class_ "tree well span12" $
                        H.toMarkup $ tree $ htmlRenderer summary

        return $ getSum (summaryFailures summary) == 0

  includeMarkup = getDataFileName >=> B.readFile >=> return . H.unsafeByteString

  item = H.li ! HA.class_ "parent_li"
              ! H.customAttribute "role" "treeitem"

  tree  = H.ul ! H.customAttribute "role" "tree"
