{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module View (
    renderDefault
  , setTitle
  , addCoffeeScript
  , addJavascript
  , addInlineJavascript
) where
import Text.Blaze.Html5 (Html, ToHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Set as S
import Data.Set (Set)
import Control.Monad.Writer
import Control.Monad.State
import Data.Monoid
import Data.List
import Coffee

type URI = String

data Type = JavaScript | CoffeeScript | Css deriving (Show, Ord, Eq)
data Where = Internal Type String | External Type URI deriving (Show, Ord, Eq)
type Title = Html
newtype View a = View (StateT Title (Writer (Set Where)) a) deriving (Monad, MonadWriter (Set Where), MonadState Title)
runView (View s) = let ((a, title), scripts) = runWriter $ runStateT s "" in (a,title,scripts)

setTitle :: Html -> View ()
setTitle = modify . const
addCoffeeScript, addJavascript :: URI -> View ()
addInlineCoffeeScript, addInlineJavascript :: String -> View ()
addCoffeeScript = tell . S.singleton . External CoffeeScript
addInlineCoffeeScript = tell . S.singleton . Internal CoffeeScript
addJavascript = tell . S.singleton . External JavaScript
addInlineJavascript = tell . S.singleton . Internal JavaScript

prepare :: Where -> Html
prepare (External JavaScript uri) =
  H.script mempty ! A.type_ "application/javascript" ! A.src (H.toValue uri)
prepare (Internal JavaScript code) =
  H.script (H.toHtml code) ! A.type_ "application/javascript"
prepare (External CoffeeScript uri) =
  prepare (External JavaScript (cacheFile uri))
prepare (Internal CoffeeScript code) =
  prepare (Internal JavaScript (cacheFile code))

compileExternalCoffee ::  Set Where -> IO [Either String String]
compileExternalCoffee = compileFiles . S.toList . S.map getURI . S.filter isExternalCoffee

data Default = Default
class Render a where
  render :: MonadIO m => a -> View Html -> m Html

instance Render Default where
  render _ = renderSite

renderDefault :: View Html -> IO Html
renderDefault = render Default

renderSite :: MonadIO m => View Html -> m Html
renderSite view = do
  liftIO $ compileExternalCoffee heads
  return $
    H.docTypeHtml $ do
      H.head $ H.title title `mappend` scripts `mappend` styles
      H.body body
    where
      scripts = S.fold (flip mappend . prepare) mempty heads
      styles = mempty
      (body, title, heads) = runView view


-- Filters
isExternalCoffee (External CoffeeScript _ ) = True
isExternalCoffee                        _   = False
isInternalCoffee (Internal CoffeeScript _ ) = True
isInternalCoffee                        _   = False

-- Getters
getURI (External _ x) = x
