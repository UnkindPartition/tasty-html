{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run a 'Tasty.TestTree' and produce an HTML file summarising the test results.
module Test.Tasty.Runners.Html (htmlRunner) where

import Prelude hiding (head, div)
import Control.Applicative (Const(..), (<$), pure)
import Control.Monad ((>=>), unless)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM (atomically, readTVar, retry)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty,mappend), (<>), Sum(Sum,getSum))
import Data.Foldable (forM_)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.ByteString as B
import Control.Monad.State (get, modify, runStateT)
import Data.Functor.Compose (Compose(Compose,getCompose))
import qualified Data.IntMap as IntMap
import Data.Proxy (Proxy(..))
import Data.Semigroup.Applicative (Traversal(Traversal,getTraversal))
import Data.Tagged (Tagged(..))
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import Test.Tasty.Runners
  ( Ingredient(TestReporter)
  , Status(Done)
  , TreeFold(foldSingle,foldGroup)
  , foldTestTree
  , trivialFold
  , resultSuccessful
  , resultDescription
  )
import Test.Tasty.Options
  ( IsOption(defaultValue,parseValue,optionName,optionHelp)
  , OptionDescription(Option)
  , lookupOption
  )
import Text.Blaze.Html5
  ( Markup
  , AttributeValue
  , (!)
  , toMarkup
  , docTypeHtml
  , head
  , meta
  , body
  , h1
  , h5
  , h6
  , div
  , p
  , ul
  , li
  , pre
  , small
  , i
  , br
  , customAttribute
  , unsafeByteString
  )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
  ( lang
  , charset
  , name
  , content
  , class_
  )
import Text.Blaze.Html.Renderer.String (renderHtml)

import Paths_tasty_html (getDataFileName)

--------------------------------------------------------------------------------
newtype HtmlPath = HtmlPath FilePath
  deriving (Typeable)

instance IsOption (Maybe HtmlPath) where
  defaultValue = Nothing
  parseValue = Just . Just . HtmlPath
  optionName = Tagged "html"
  optionHelp = Tagged "A file path to store the test results in Html"


--------------------------------------------------------------------------------
data Summary = Summary { summaryFailures :: Sum Int
                       , summarySuccesses :: Sum Int
                       , htmlRenderer :: Markup
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
htmlRunner :: Ingredient
htmlRunner = TestReporter optionDescription runner
 where
  optionDescription = [ Option (Proxy :: Proxy (Maybe HtmlPath)) ]
  runner options testTree = do
    HtmlPath path <- lookupOption options

    return $ \statusMap ->

      let
        runTest _ testName _ = Traversal $ Compose $ do
          ix <- get

          summary <- lift $ atomically $ do
            status <- readTVar $
              fromMaybe (error "Attempted to lookup test by index outside bounds") $
              IntMap.lookup ix statusMap

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
              Done result
                | resultSuccessful result -> pure $
                    mkSuccess $ resultDescription result
                | otherwise -> pure $ mkFailure $ resultDescription result
              -- Otherwise the test has either not been started or is currently
              -- executing
              _ -> retry

          Const summary <$ modify (+1)

        runGroup groupName children = Traversal $ Compose $ do
          Const soFar <- getCompose $ getTraversal children
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
          flip runStateT 0 $ getCompose $ getTraversal $
          foldTestTree
            trivialFold { foldSingle = runTest
                        , foldGroup = runGroup
                        }
            options
            testTree

        bootStrapCSS  <- includeMarkup "data/bootstrap-combined.min.css"
        customCSS     <- includeMarkup "data/style.css"
        jquery        <- includeScript "data/jquery-2.1.0.min.js"
        bootstrapTree <- includeScript "data/bootstrap-tree.js"

        writeFile path $
          renderHtml $
            docTypeHtml ! lang "en" $ do
              head $ do
                meta ! charset "utf-8"
                H.title "Tasty Test Results"
                meta ! name "viewport"
                     ! content "width=device-width, initial-scale=1.0"
                H.style bootStrapCSS
                H.style customCSS
                jquery
                bootstrapTree
              body $ div ! class_ "container" $ do
                h1 ! class_ "text-center" $ "Tasty Test Results"
                div ! class_ "row" $
                  if summaryFailures summary > Sum 0
                    then
                      div ! class_ "alert alert-block alert-error" $
                        p ! class_ "lead text-center" $ do
                          toMarkup . getSum $ summaryFailures summary
                          " out of " :: Markup
                          toMarkup tests
                          " tests failed"
                    else
                      div ! class_ "alert alert-block alert-success" $
                        p ! class_ "lead text-center" $ do
                          "All " :: Markup
                          toMarkup tests
                          " tests passed"

                div ! class_ "row" $
                  div ! class_ "tree well" $
                    toMarkup $ tree $ htmlRenderer summary

        return $ getSum (summaryFailures summary) == 0

  includeMarkup =
    getDataFileName >=> B.readFile >=> return . unsafeByteString

  includeScript =
    getDataFileName >=> B.readFile >=> \bs ->
    return . unsafeByteString $ "<script>" <> bs <> "</script>"

  item = li ! class_ "parent_li"
            ! customAttribute "role" "treeitem"

  tree  = ul ! customAttribute "role" "tree"

branch :: String
       -> Bool
       -> Maybe (String, AttributeValue)
       -> AttributeValue
       -> AttributeValue
       -> AttributeValue
       -> Markup
branch name_ isBig mdesc icon clas_ text = do
  H.span ! class_ clas_ $
    i ! class_ icon $ ""
  (if isBig then h5 else h6) ! class_ text $
    toMarkup $ "  " ++ name_
  forM_ mdesc $ \(desc,desca) ->
    unless (null desc) $ do
      br
      pre $ small ! class_ desca $ toMarkup desc
