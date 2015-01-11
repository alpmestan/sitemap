{-# LANGUAGE OverloadedStrings #-}
module Web.Sitemap
  (
  -- * A sitemap 'Entry', with lenses
    Entry(..)
  , loc
  , lastmod
  , changefreq
  , priority

  -- * Parsing a sitemap
  , parseSitemap
  ) where

import Control.Lens hiding (elements)
import Data.Text.Read (double)
import Text.Taggy.Lens
import Web.Sitemap.Types

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

readDouble :: T.Text -> Maybe Double
readDouble = either (const Nothing) (Just . fst) . double

parseSitemap :: LT.Text -> [Entry]
parseSitemap src =
  src ^.. html
        . elements
        . named (only "urlset")
        . elements
        . named (only "url")
        . to entry

entry :: HasElements s => s -> Entry
entry u = 
  Entry (u ^?! elements . named (only "loc") . contents)
        (u ^? elements . named (only "lastmod") . contents)
        (u ^? elements . named (only "changefreq") . contents)
        (u ^? elements . named (only "priority") . contents . to readDouble . _Just)

