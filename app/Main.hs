{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad (forM, forM_)
import Text.Pandoc
  ( Block(..)
  , Meta
  , Pandoc(..)
  , def
  , handleError
  , pandocExtensions
  , readMarkdown
  , readerExtensions
  , runIO
  , runPure
  , writeHtml5
  , writeHtml5String
  )

import Text.Blaze.Html (customAttribute)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
import Data.List (sort)

defaultHeadingLevel = 3

extractQuestions :: [Block] -> [[Block]]
extractQuestions ((Header 1 attr text):tail) =
  let isH1 :: Block -> Bool
      isH1 (Header 1 _ _) = True
      isH1 _ = False
      (content, remaining) = break isH1 tail
   in (Header 1 attr text : content) : extractQuestions remaining
extractQuestions [] = []

extractQuestions' [] = []

data Question = Question
  { title :: String
  , slug :: String
  , tags :: [String]
  , inner :: H.Html
  }

blocks2Questions :: Meta -> [[Block]] -> IO [Question]
blocks2Questions meta = mapM convert
  where
    convert :: [Block] -> IO Question
    convert ((Header 1 (slug, tags, kv) text):content) = do
      let content' = map lowerHeadings content
      title <-
        runIO (writeHtml5String def (Pandoc meta [Plain text])) >>= handleError
      html <- runIO (writeHtml5 def (Pandoc meta content')) >>= handleError
      return
        $ Question
            { title = T.unpack title
            , slug = T.unpack slug
            , tags = map T.unpack tags
            , inner = H.div ! A.class_ "question"
                            $ do
                              H.h3 ! A.id (H.textValue slug) $ do
                                H.toHtml title
                              H.p $ do
                                forM_ (sort tags) $ \tag -> do
                                  " "
                                  H.span ! A.class_ "badge text-bg-secondary" $ H.toHtml tag
                              html

            }
    lowerHeadings :: Block -> Block
    lowerHeadings (Header lvl attr text) =
      Header (lvl + defaultHeadingLevel - 1) attr text
    lowerHeadings other = other

index :: [Question] -> [String] -> H.Html
index questions attributes =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta
        ! A.name "viewport"
        ! A.content "width=device-width, initial-scale=1"
      H.title "Cybersec training"
      H.link
        ! A.href
            "https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css"
        ! A.rel "stylesheet"
        ! customAttribute
            "integrity"
            "sha384-sRIl4kxILFvY47J16cr9ZwB07vP4J8+LH7qKQnuqkuIAvNWLzeN8tE5YBujZqJLB"
        ! customAttribute "crossorigin" "anonymous"
    H.body $ do
      H.nav ! A.class_ "navbar navbar-expand-lg bg-body-tertiary sticky-top" $ do
        H.div ! A.class_ "container-fluid" $ do
          H.a ! A.class_ "navbar-brand" $ "Cybersec Training"
          H.button
            ! A.class_ "navbar-toggler"
            ! A.type_ "button"
            ! customAttribute "data-bs-toggle" "collapse"
            ! customAttribute "data-bs-target" "#navbarSupportedContent"
            ! customAttribute "aria-controls" "navbarSupportedContent"
            ! customAttribute "aria-expanded" "false"
            ! customAttribute "aria-label" "Toggle navigation"
            $ H.span ! A.class_ "navbar-toggler-icon"
            $ ""
          H.div
            ! A.class_ "collapse navbar-collapse"
            ! A.id "navbarSupportedContent" $ do
            H.ul ! A.class_ "navbar-nav me-auto mb-2 mb-lg-0" $ do
              H.li ! A.class_ "nav-item"
                $ H.a
                    ! A.class_ "nav-link"
                    ! A.href "#offcanvas"
                    ! customAttribute "data-bs-toggle" "offcanvas"
                $ "Wszystkie pytania"
                      -- $ forM_ attributes $ \attr -> H.li ! A.class_ "nav-item"
                         --                                $ H.a ! A.class_ "nav-link" ! A.href "#"  $ H.toHtml attr
            H.div ! A.class_ "d-flex" ! A.role "search" $ do
              H.input
                ! A.class_ "form-control me-2"
                ! A.type_ "search"
                ! A.placeholder "Szukaj"
                  -- H.button ! A.class_ "btn btn-outline-success" $ "Search"
      H.main
        ! A.class_ "container"
        ! customAttribute "data-bs-spy" "scroll"
        ! customAttribute "data-bs-target" "#questions-nav"
        $ forM_ questions inner
      H.div
        ! A.class_ "offcanvas offcanvas-start"
        ! customAttribute "data-bs-scroll" "true"
        ! customAttribute "data-bs-backdrop" "false"
        ! A.tabindex "-1"
        ! A.id "offcanvas" $ do
        H.div ! A.class_ "offcanvas-header" $ do
          H.div ! A.class_ "offcanvas-title" $ "Wszystkie pytania"
          H.button
            ! A.type_ "button"
            ! A.class_ "btn-close"
            ! customAttribute "data-bs-dismiss" "offcanvas"
            ! customAttribute "aria-label" "Close"
            $ ""
        H.div ! A.class_ "offcanvas-body" $ do
          H.nav ! A.class_ "list-group" ! A.id "questions-nav" $ do
            forM_ questions $ \question -> do
              H.a
                ! A.href (H.stringValue (printf "#%s" (slug question)))
                ! A.class_ "list-group-item list-group-item-action"
                $ H.toHtml
                $ title question
      H.script
        ! A.src
            "https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/js/bootstrap.bundle.min.js"
        ! customAttribute
            "integrity"
            "sha384-FKyoEForCGlyvwx9Hj09JcYn3nv7wiPVlz7YYwJrWVcXK/BmnVDxM+D2scQbITxI"
        ! customAttribute "crossorigin" "anonymous"
        $ ""

main :: IO ()
main = do
  file <- TIO.readFile "src/rb.md"
  (Pandoc meta blocks) <-
    runIO (readMarkdown def {readerExtensions = pandocExtensions} file)
      >>= handleError
  questions <- blocks2Questions meta $ extractQuestions blocks
  writeFile "index.html" $ renderHtml $ index questions ["zwierzęta", "kamienie"]
