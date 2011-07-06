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

renderSite ::  Html -> View Html -> IO Html
renderSite title view = do
  compileFiles $ S.toList $ S.map getURI $ S.filter isExternalCoffee heads
  return $ 
    H.docTypeHtml $ do
      H.head $ H.title title `mappend` scripts `mappend` styles
      H.body $ body
    where
      externaljs x = H.script mempty ! A.type_ "application/javascript" ! A.src x
      inlinejs x = H.script x ! A.type_ "application/javascript"
      scripts = S.fold (flip mappend . externaljs . H.toValue . getURI) mempty (S.filter isExternalJS heads) `mappend`
                S.fold (flip mappend . inlinejs . H.toHtml . getInline) mempty (S.filter isInternalJS heads) `mappend`
                S.fold (flip mappend . externaljs . H.toValue . cacheFile . getURI) mempty (S.filter isExternalCoffee heads)
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
isExternalJS (External JavaScript       _ ) = True
isExternalJS                            _   = False
isExternalCoffee (External CoffeeScript _ ) = True
isExternalCoffee                        _   = False
isExternalCss (External Css             _ ) = True
isExternalCss                           _   = False
isInternalJS (Internal JavaScript       _ ) = True
isInternalJS                            _   = False
isInternalCoffee (Internal CoffeeScript _ ) = True
isInternalCoffee                        _   = False
isInternalCss (Internal Css             _ ) = True
isInternalCss                           _   = False
-- Getters
getURI (External _ x)    = x
getInline (Internal _ x) = x
