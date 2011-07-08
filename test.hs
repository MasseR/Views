{-# LANGUAGE OverloadedStrings #-}
module Main where
import View
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.Pretty as P

view = renderDefault $ do
  setTitle "foo"
  addCoffeeScript "foo.coffee"
  addJavascript "js/jquery.js"
  addInlineJavascript "alert(\"Hello views\");"
  return $
    H.div $ do
      H.h2 "Hello views"
      H.p "This is a test on views"
main = (putStrLn . P.renderHtml) =<< view
