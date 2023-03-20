{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run a 'Tasty.TestTree' and produce an HTML file summarising the test results.
module Test.Tasty.Runners.Html
  ( HtmlPath(..)
  , htmlRunner
  , AssetsPath(..)
  ) where

import Control.Applicative (Const(..))
import Control.Monad ((>=>), unless, forM_, when)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM (atomically, readTVar)
import qualified Control.Concurrent.STM as STM(retry)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(Sum,getSum))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.FilePath ((</>))
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.ByteString as B
import Control.Monad.State (StateT, evalStateT, liftIO)
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
import qualified Test.Tasty.Ingredients as Tasty
import Test.Tasty.Options as Tasty
import Text.Blaze.Html5 (Markup, AttributeValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Printf (printf)

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

-- | Path where external assets will be looked up
newtype AssetsPath = AssetsPath FilePath deriving (Typeable)

-- | Assets 'Option' for the HTML 'Ingredient'.
instance IsOption (Maybe AssetsPath) where
    defaultValue = Nothing
    parseValue = Just . Just . AssetsPath
    optionName = Tagged "assets"
    optionHelp = Tagged "Directory where HTML assets will be looked up. \
                        \If not given the assets will be inlined within the \
                        \HTML file. \
                        \The following files must be present: \
                        \`bootstrap.min.css`, `bootstrap.min.js`, `jquery-2.1.1.min.js`"

{-| To run tests using this ingredient, use 'Tasty.defaultMainWithIngredients',
    passing 'htmlRunner' as one possible ingredient. This ingredient will run
    tests if you pass the @--html@ command line option. For example,
    @--html=results.html@ will run all the tests and generate @results.html@ as
    output.

    Note enabling @--html@ will ignore all ingredients following 'htmlRunner'.
    If you want to produce the HTML report /in addition/ to other outputs, you can
    use 'Tasty.composeReporters', as in

    > main = defaultMainWithIngredients ingredients tests
    >   where ingredients = [ listingTests, htmlRunner `composeReporters` consoleTestReporter ]

-}
htmlRunner :: Ingredient
htmlRunner = TestReporter optionDescription $ \options testTree -> do
  HtmlPath htmlPath <- lookupOption options
  let mAssetsPath = lookupOption options
  return $ \statusMap -> do
    Const summary <- flip evalStateT 0 $ getCompose $ getTraversal $
      Tasty.foldTestTree
        Tasty.trivialFold { Tasty.foldSingle = runTest statusMap
                          , Tasty.foldGroup  = runGroup
                          }
        options
        testTree

    return $ \time -> do
      generateHtml summary time htmlPath mAssetsPath
      return $ getSum (summaryFailures summary) == 0

 where
  optionDescription = [ Option (Proxy :: Proxy (Maybe HtmlPath))
                      , Option (Proxy :: Proxy (Maybe AssetsPath))
                      ]

-- Silence unused import warning
_onlyUsedByHaddock :: ()
_onlyUsedByHaddock = ()
  where _ = Tasty.composeReporters


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

instance Semigroup Summary where
  (<>) = mappenddefault

instance Monoid Summary where
  mempty = memptydefault
  mappend = (<>)

-- | A 'Traversal' composed of a 'Summary' and a test count.
type SummaryTraversal = Traversal (Compose (StateT Int IO) (Const Summary))

-- ** Test folding

-- | To be used for an individual test when when folding the final 'TestTree'.
runTest :: IsTest t
        => StatusMap -> OptionSet -> TestName -> t -> SummaryTraversal
runTest statusMap _ testName _ = Traversal $ Compose $ do
  ix <- State.get

  result <- lift $ atomically $ do
    status <- readTVar $
      fromMaybe (error "Attempted to lookup test by index outside bounds") $
      IntMap.lookup ix statusMap

    case status of
      -- If the test is done, return the result
      Done result -> return result
      -- Otherwise the test has either not been started or is currently
      -- executing
      _ -> STM.retry

  -- Generate HTML for the test
  msg <- liftIO . Tasty.formatMessage . Tasty.resultDescription $ result
  let time = Tasty.resultTime result
      summary = if Tasty.resultSuccessful result
                then mkSuccess (testName, time) msg
                else mkFailure (testName, time) msg

  Const summary <$ State.modify (+1)

-- | To be used for a 'TestGroup' when folding the final 'TestTree'.
runGroup :: OptionSet -> TestName -> SummaryTraversal -> SummaryTraversal
runGroup _opts groupName children = Traversal $ Compose $ do
  Const soFar <- getCompose $ getTraversal children

  let (extra,text) = if summaryFailures soFar > Sum 0
                        then ( "btn-danger"
                             , "text-danger"
                             )
                        else ( "btn-success"
                             , "text-success"
                             )
      grouped = testGroupMarkup groupName extra text $
                  treeMarkup $ htmlRenderer soFar

  return $ Const soFar { htmlRenderer = grouped }

-- ** HTML

-- | Generates the final HTML report.
generateHtml :: Summary  -- ^ Test summary.
             -> Tasty.Time -- ^ Total run time.
             -> FilePath -- ^ Where to write.
             -> Maybe AssetsPath -- ^ Path to external assets
             -> IO ()
generateHtml summary time htmlPath mAssetsPath = do
      -- Helpers to load external assets
  let getRead = getDataFileName >=> B.readFile
      includeMarkup = getRead >=> return . H.unsafeByteString
      -- blaze-html 'script' doesn't admit HTML inside
      includeScript = getRead >=> \bs ->
        return . H.unsafeByteString $ "<script>" <> bs <> "</script>"

  -- Only used when no external assets path specified
  bootStrapCss      <- includeMarkup "data/bootstrap/dist/css/bootstrap.min.css"
  jQueryJs          <- includeScript "data/jquery-2.1.1.min.js"
  bootStrapJs       <- includeScript "data/bootstrap/dist/js/bootstrap.min.js"
  scriptJs          <- includeScript "data/script.js"

  TIO.writeFile htmlPath $
    renderHtml $
      H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
          H.meta ! A.charset "utf-8"
          H.meta ! A.name "viewport"
                 ! A.content "width=device-width, initial-scale=1.0"
          H.title "Tasty Test Results"

          case mAssetsPath of
            Nothing -> do H.style bootStrapCss
                          jQueryJs
                          bootStrapJs
            Just (AssetsPath assetsPath) -> do
              H.link ! A.rel "stylesheet"
                     ! A.href (H.toValue $ assetsPath </> "bootstrap.min.css")
              forM_ ["bootstrap.min.js", "jquery-2.1.1.min.js", "bootstrap.min.js"] $ \str ->
                H.script ! A.src (H.toValue $ assetsPath </> str ) $ mempty

        H.body $ do
          H.div ! A.class_ "container" $ do
            H.h1 ! A.class_ "text-center" $ "Tasty Test Results"
            H.div ! A.class_ "row" $
              if summaryFailures summary > Sum 0
                then
                  H.div ! A.class_ "alert alert-danger" $
                    H.p ! A.class_ "lead text-center" $ do
                      H.toMarkup . getSum $ summaryFailures summary
                      " out of " :: Markup
                      H.toMarkup tests
                      " tests failed" :: Markup
                      H.span ! A.class_ "text-muted" $ H.toMarkup (formatTime time)
                else
                  H.div ! A.class_ "alert alert-success" $
                    H.p ! A.class_ "lead text-center" $ do
                      "All " :: Markup
                      H.toMarkup tests
                      " tests passed" :: Markup
                      H.span ! A.class_ "text-muted" $ H.toMarkup (formatTime time)

            H.div ! A.class_ "row" $
              H.div ! A.class_ "well" $
                H.toMarkup $ treeMarkup $ htmlRenderer summary
          scriptJs

 where
  -- Total number of tests
  tests = getSum $ summaryFailures summary <> summarySuccesses summary

-- | Set the 'htmlRenderer' of a 'Summary' with the given 'Markup'.
mkSummary :: Markup -> Summary
mkSummary contents = mempty { htmlRenderer = itemMarkup contents }

-- | Create an HTML 'Summary' with a test success.
mkSuccess :: (TestName, Tasty.Time)
          -> String -- ^ Description for the test.
          -> Summary
mkSuccess nameAndTime desc =
      ( mkSummary $ testItemMarkup
          nameAndTime
          (desc, "text-muted")
          "glyphicon-ok-sign"
          "btn-success"
          "text-success"
      ) { summarySuccesses = Sum 1 }

-- | Create an HTML 'Summary' with a test failure.
mkFailure :: (TestName, Tasty.Time)
          -> String -- ^ Description for the test.
          -> Summary
mkFailure nameAndTime desc =
      ( mkSummary $ testItemMarkup
          nameAndTime
          (desc, "text-danger")
          "glyphicon-remove-sign"
          "btn-danger"
          "text-danger"
      ) { summaryFailures = Sum 1 }

-- | Markup representing the branching of a /tree/.
treeMarkup :: Markup -> Markup
treeMarkup rest =
  H.div ! A.class_ "media collapse in" $
    H.ul ! A.class_ "media-list" $
      rest

-- | Markup representing an /item/ in a /tree/.
itemMarkup :: Markup -> Markup
itemMarkup = H.li ! A.class_ "media"

type CssDescription = (String, AttributeValue)
type CssIcon  = AttributeValue
type CssExtra = AttributeValue
type CssText  = AttributeValue

-- | Markup for a button.
buttonMarkup :: CssExtra -> CssIcon -> Markup
buttonMarkup extra icon =
  H.button ! A.type_ "button"
           ! A.class_ ("btn btn-xs pull-left media-object " <> extra)
           $ H.span ! A.class_ ("glyphicon " <> icon) $ ""

-- | Markup for a test group.
testGroupMarkup :: TestName -> CssExtra -> CssText -> Markup -> Markup
testGroupMarkup groupName extra text body =
    H.li ! A.class_ "media" $ do
      buttonMarkup (extra <> " collapsible") "glyphicon-folder-open"
      H.div ! A.class_ "media-body" $ do
        H.h4 ! A.class_ ("media-heading " <> text) $
          H.toMarkup groupName
        body

-- | Markup for a single test.
testItemMarkup :: (TestName, Tasty.Time)
               -> CssDescription
               -> CssIcon
               -> CssExtra
               -> CssText
               -> Markup
testItemMarkup (testName,time) (desc,desca) icon extra text = do
  buttonMarkup extra icon
  H.div ! A.class_ "media-body" $ do
    H.h5 ! A.class_ ("media-heading " <> text) $ do
      H.toMarkup testName
      when (time >= 0.01) $
        H.span ! A.class_ "text-muted" $ H.toMarkup (formatTime time)

    unless (null desc) $
      H.pre $ H.small ! A.class_ desca $ H.toMarkup desc

formatTime :: Tasty.Time -> String
formatTime = printf " (%.2fs)"

-- vim: textwidth=79 shiftwidth=2
