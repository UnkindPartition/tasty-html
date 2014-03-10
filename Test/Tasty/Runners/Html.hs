{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run a 'Tasty.TestTree' and produce an HTML file summarising the test results.
module Test.Tasty.Runners.Html (htmlRunner) where

import Control.Applicative
import Data.Foldable (forM_)
import Control.Monad ((>=>), unless)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..), Sum(..), (<>))
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
  @--html=results.html@ will run all the tests and generate @results.html@ as output.

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

            let leave = branch testName False

                mkSummary contents = mempty { htmlRenderer = item contents }

                mkSuccess desc =
                  ( mkSummary $
                      leave (Just (desc, "muted"))
                            "icon-ok-sign"
                            "badge badge-success"
                            "text-success"
                  ) { summarySuccesses = Sum 1 }

                mkFailure desc =
                  ( mkSummary $
                      leave (Just (desc, "text-error"))
                            "icon-remove-sign"
                            "badge badge-important"
                            "text-error"
                  ) { summaryFailures = Sum 1 }

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
          let groupBranch = branch groupName True Nothing "icon-folder-open"
              grouped = item $ do
                if summaryFailures soFar > Sum 0
                  then groupBranch "badge badge-important" "text-error"
                  else groupBranch "badge badge-success" "text-success"
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

        css           <- includeMarkup "data/bootstrap-combined.min.css"
        style         <- includeMarkup "data/style.css"
        jquery        <- includeScript "data/jquery-2.1.0.min.js"
        bootstrapTree <- includeScript "data/bootstrap-tree.js"

        writeFile path $
          renderHtml $
            H.docTypeHtml ! HA.lang "en" $ do
              H.head $ do
                H.meta ! HA.charset "utf-8"
                H.title "Tasty Test Results"
                H.meta ! HA.name "viewport"
                       ! HA.content "width=device-width, initial-scale=1.0"
                H.style css
                H.style style
                jquery
                bootstrapTree
              H.body $ H.div ! HA.class_ "container" $ do
                H.h1 ! HA.class_ "text-center" $ "Tasty Test Results"
                H.div ! HA.class_ "row" $
                  if summaryFailures summary > Sum 0
                    then
                      H.div ! HA.class_ "alert alert-block alert-error" $
                        H.p ! HA.class_ "lead text-center" $ do
                          H.toMarkup . getSum $ summaryFailures summary
                          " out of " :: H.Markup
                          H.toMarkup tests
                          " tests failed"
                    else
                      H.div ! HA.class_ "alert alert-block alert-success" $
                        H.p ! HA.class_ "lead text-center" $ do
                          "All " :: H.Markup
                          H.toMarkup tests
                          " tests passed"

                H.div ! HA.class_ "row" $
                  H.div ! HA.class_ "tree well" $
                    H.toMarkup $ tree $ htmlRenderer summary

        return $ getSum (summaryFailures summary) == 0

  includeMarkup =
    getDataFileName >=> B.readFile >=> return . H.unsafeByteString

  includeScript =
    getDataFileName >=> B.readFile >=> \bs ->
    return . H.unsafeByteString $ "<script>" <> bs <> "</script>"

  item = H.li ! HA.class_ "parent_li"
              ! H.customAttribute "role" "treeitem"

  tree  = H.ul ! H.customAttribute "role" "tree"

branch :: String
       -> Bool
       -> Maybe (String, H.AttributeValue)
       -> H.AttributeValue
       -> H.AttributeValue
       -> H.AttributeValue
       -> H.Markup
branch name isBig mdesc icon class_ text = do
  H.span ! HA.class_ class_ $
    H.i ! HA.class_ icon $ ""
  (if isBig then H.h5 else H.h6) ! HA.class_ text $
    H.toMarkup $ "  " ++ name
  forM_ mdesc $ \(desc,desca) ->
    unless (null desc) $ do
      H.br
      H.pre $ H.small ! HA.class_ desca $ H.toMarkup desc
