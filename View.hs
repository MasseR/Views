{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module View where
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.Pretty as P
import qualified Data.Set as S
import Data.Set (Set)
import Control.Monad.Writer
import Data.Monoid
import Data.List
import Coffee

type URI = String

data Type = JavaScript | CoffeeScript | Css deriving (Show, Ord, Eq)
data Where = Internal Type String | External Type URI deriving (Show, Ord, Eq)
newtype View a = View {runView :: Writer (Set Where) a} deriving (Monad, MonadWriter (Set Where))

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

renderSite ::  Html -> View Html -> IO Html
renderSite title view = do
  compileFiles $ S.toList $ S.map getURI $ S.filter isExternalCoffee heads
  return $
    H.docTypeHtml $ do
      H.head $ H.title title `mappend` scripts `mappend` styles
      H.body $ body
    where
      scripts = S.fold (flip mappend . prepare) mempty heads
      styles = mempty
      (body, heads) = runWriter $ runView view

test = renderSite "Hello views" $ do
  addCoffeeScript "foo.coffee"
  addJavascript "js/jquery.js"
  addInlineJavascript "alert(\"Hello views\");"
  return $
    H.div $ do
      H.h2 "Hello views"
      H.p "This is a test on views"

-- Filters
isExternalCoffee (External CoffeeScript _ ) = True
isExternalCoffee                        _   = False
isInternalCoffee (Internal CoffeeScript _ ) = True
isInternalCoffee                        _   = False

-- Getters
getURI (External _ x) = x
