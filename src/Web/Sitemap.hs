{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module:      Web.Sitemap
-- Copyright:   (c) 2015 Alp Mestanogullari
-- License:     BSD3
-- Maintainer:  Alp Mestanogullari <alpmestan@gmail.com>
-- Stability:   experimental
--
-- Parse sitemaps and extract information with lenses.
--
------------------------------------------------------------------------------

module Web.Sitemap
  ( -- * Usage
    -- $usage

    -- * Parsing
    parseSitemap

    -- * A sitemap 'Entry', with lenses
  , Entry(..)
  , loc
  , lastmod
  , changefreq
  , priority
  , URL
  ) where

import Control.Lens hiding (elements)
import Data.Text.Read (double)
import Text.Taggy.Lens
import Web.Sitemap.Types

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

readDouble :: T.Text -> Maybe Double
readDouble = either (const Nothing) (Just . fst) . double

-- | Turn a sitemap into a list of 'Entry's.
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

-- $usage
--
-- Simply feed 'parseSitemap' with a sitemap as
-- some lazy 'LT.Text' (that you can fetch from
-- a distant web server or locally) and then
-- extract all the information you want using the
-- lenses.
--
-- > parseSitemap sitemapfilecontent ^.. traverse.loc
--
-- would hand you back all the 'URL's the sitemap points to.
