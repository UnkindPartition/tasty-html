{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run a 'Tasty.TestTree' and produce an HTML file summarising the test results.
module Test.Tasty.Runners.Html
  ( HtmlPath(..)
  , htmlRunner
  ) where

import Control.Applicative (Const(..), (<$), pure)
import Control.Monad ((>=>), unless)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM (atomically, readTVar)
import qualified Control.Concurrent.STM as STM(retry)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty,mappend), (<>), Sum(Sum,getSum))
import Data.Foldable (forM_)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.ByteString as B
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State (get, modify)
import Data.Functor.Compose (Compose(Compose,getCompose))
import qualified Data.IntMap as IntMap
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import Test.Tasty.Runners
  ( Ingredient(TestReporter)
  , Status(Done)
  , StatusMap
  , Traversal(Traversal,getTraversal)
  )
import Test.Tasty.Providers (IsTest, TestName)
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.Options as Tasty
import Text.Blaze.Html5 (Markup, AttributeValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Paths_tasty_html (getDataFileName)

-- * Exported

-- | Path where the HTML will be rendered.
newtype HtmlPath = HtmlPath FilePath deriving (Typeable)

-- | HTML 'Option' for the HTML 'Ingredient'.
instance IsOption (Maybe HtmlPath) where
  defaultValue = Nothing
  parseValue = Just . Just . HtmlPath
  optionName = Tagged "html"
  optionHelp = Tagged "A file path to store the test results in HTML"

{-| To run tests using this ingredient, use 'Tasty.defaultMainWithIngredients',
    passing 'htmlRunner' as one possible ingredient. This ingredient will run
    tests if you pass the @--html@ command line option. For example,
    @--html=results.html@ will run all the tests and generate @results.html@ as
    output.
-}
htmlRunner :: Ingredient
htmlRunner = TestReporter optionDescription $ \options testTree -> do
  HtmlPath path <- lookupOption options
  return $ \statusMap -> do
    Const summary <- flip evalStateT 0 $ getCompose $ getTraversal $
      Tasty.foldTestTree
        Tasty.trivialFold { Tasty.foldSingle = runTest statusMap
                          , Tasty.foldGroup  = runGroup
                          }
        options
        testTree

    generateHtml summary path

    return $ getSum (summaryFailures summary) == 0
 where
  optionDescription = [ Option (Proxy :: Proxy (Maybe HtmlPath)) ]

-- * Internal

-- ** Types
--
{-| Includes the number of successful and failed tests and the 'Markup' to
    render the results of a test run.
-}
data Summary = Summary { summaryFailures :: Sum Int
                       , summarySuccesses :: Sum Int
                       , htmlRenderer :: Markup
                       } deriving (Generic)

instance Monoid Summary where
  mempty = memptydefault
  mappend = mappenddefault

-- | A 'Traversal' composed of a 'Summary' and a test count.
type SummaryTraversal = Traversal (Compose (StateT Int IO) (Const Summary))

-- ** Test folding

-- | To be used for an individual test when when folding the final 'TestTree'.
runTest :: IsTest t
        => StatusMap -> OptionSet -> TestName -> t -> SummaryTraversal
runTest statusMap _ testName _ = Traversal $ Compose $ do
  ix <- State.get

  summary <- lift $ atomically $ do
    status <- readTVar $
      fromMaybe (error "Attempted to lookup test by index outside bounds") $
      IntMap.lookup ix statusMap

    case status of
      -- If the test is done, generate HTML for it
      Done result
        | Tasty.resultSuccessful result -> pure $
            mkSuccess testName $ Tasty.resultDescription result
        | otherwise ->
            pure $ mkFailure testName $ Tasty.resultDescription result
      -- Otherwise the test has either not been started or is currently
      -- executing
      _ -> STM.retry

  Const summary <$ State.modify (+1)

-- | To be used for a 'TestGroup' when folding the final 'TestTree'.
runGroup :: TestName -> SummaryTraversal -> SummaryTraversal
runGroup groupName children = Traversal $ Compose $ do
  Const soFar <- getCompose $ getTraversal children

  let render = testGroupMarkup groupName
      grouped = itemMarkup $ do
        if summaryFailures soFar > Sum 0
          then render "badge badge-important" "text-error"
          else render "badge badge-success"   "text-success"
        treeMarkup $ htmlRenderer soFar

  pure $ Const soFar { htmlRenderer = grouped }

-- ** HTML

-- | Generates the final HTML report.
generateHtml :: Summary  -- ^ Test summary.
             -> FilePath -- ^ Where to write.
             -> IO ()
generateHtml summary path = do
      -- Helpers to load external assets
  let getRead = getDataFileName >=> B.readFile
      includeMarkup = getRead >=> return . H.unsafeByteString
      -- blaze-html 'script' doesn't admit HTML inside
      includeScript = getRead >=> \bs ->
        return . H.unsafeByteString $ "<script>" <> bs <> "</script>"

  bootStrapCSS  <- includeMarkup "data/bootstrap-combined.min.css"
  customCSS     <- includeMarkup "data/style.css"
  jquery        <- includeScript "data/jquery-2.1.0.min.js"
  bootstrapTree <- includeScript "data/bootstrap-tree.js"

  TIO.writeFile path $
    renderHtml $
      H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
          H.meta ! A.charset "utf-8"
          H.title "Tasty Test Results"
          H.meta ! A.name "viewport"
                 ! A.content "width=device-width, initial-scale=1.0"
          H.style bootStrapCSS
          H.style customCSS
          jquery
          bootstrapTree

        H.body $ H.div ! A.class_ "container" $ do
          H.h1 ! A.class_ "text-center" $ "Tasty Test Results"
          H.div ! A.class_ "row" $
            if summaryFailures summary > Sum 0
              then
                H.div ! A.class_ "alert alert-block alert-error" $
                  H.p ! A.class_ "lead text-center" $ do
                    H.toMarkup . getSum $ summaryFailures summary
                    " out of " :: Markup
                    H.toMarkup tests
                    " tests failed"
              else
                H.div ! A.class_ "alert alert-block alert-success" $
                  H.p ! A.class_ "lead text-center" $ do
                    "All " :: Markup
                    H.toMarkup tests
                    " tests passed"

          H.div ! A.class_ "row" $
            H.div ! A.class_ "tree well" $
              H.toMarkup $ treeMarkup $ htmlRenderer summary
 where
  -- Total number of tests
  tests = getSum $ summaryFailures summary <> summarySuccesses summary

-- | Set the 'htmlRenderer' of a 'Summary' with the given 'Markup'.
mkSummary :: Markup -> Summary
mkSummary contents = mempty { htmlRenderer = itemMarkup contents }

-- | Create an HTML 'Summary' with a test success.
mkSuccess :: TestName
          -> String -- ^ Description for the test.
          -> Summary
mkSuccess testName desc =
      ( mkSummary $ testItemMarkup
          testName
          (Just (desc, "muted"))
          "icon-ok-sign"
          "badge badge-success"
          "text-success"
      ) { summarySuccesses = Sum 1 }

-- | Create an HTML 'Summary' with a test failure.
mkFailure :: TestName
          -> String -- ^ Description for the test.
          -> Summary
mkFailure testName desc =
      ( mkSummary $ testItemMarkup
          testName
          (Just (desc, "text-error"))
          "icon-remove-sign"
          "badge badge-important"
          "text-error"
      ) { summaryFailures = Sum 1 }

-- | Create a @bootstrap-tree@ HTML /tree/.
treeMarkup :: Markup -> Markup
treeMarkup  = H.ul ! H.customAttribute "role" "tree"

-- | Create a @bootstrap-tree@ HTML /treeitem/
itemMarkup :: Markup -> Markup
itemMarkup = H.li ! A.class_ "parent_li"
                  ! H.customAttribute "role" "treeitem"

type MaybeCssDescription = Maybe (String, AttributeValue)
type CssIcon  = AttributeValue
type CssExtra = AttributeValue
type CssText  = AttributeValue

{-| Helper function to generate an HTML tag corresponding to a either a node or
    a leave in a @bootstrap-tree* HTML tree.
-}
branchMarkup :: String
       -- ^ Name of the branch.
       -> Bool
       -- ^ Whether the text will be big or not.
       -> MaybeCssDescription
       -- ^ Description to add to the branch if applicable.
       -> CssIcon
       -- ^ CSS corresponding to the icon for the branch.
       -> CssExtra
       -- ^ Extra CSS classes for the branch.
       -> CssText
       -- ^ CSS class for text inside the branch.
       -> Markup
branchMarkup name_ isBig mdesc icon extra text = do
  H.span ! A.class_ extra $
    H.i ! A.class_ icon $ ""
  (if isBig then H.h5 else H.h6) ! A.class_ text $
    H.toMarkup $ "  " ++ name_
  forM_ mdesc $ \(desc,desca) ->
    unless (null desc) $ do
      H.br
      H.pre $ H.small ! A.class_ desca $ H.toMarkup desc

-- | Markup generator for a test item.
testItemMarkup :: TestName
               -> MaybeCssDescription
               -> CssIcon
               -> CssExtra
               -> CssText
               -> Markup
testItemMarkup testName = branchMarkup testName False

-- | Markup generator for a test group.
testGroupMarkup :: TestName -> CssExtra -> CssText -> Markup
testGroupMarkup groupName =
  branchMarkup groupName True Nothing "icon-folder-open"

-- vim: textwidth=79 shiftwidth=2
