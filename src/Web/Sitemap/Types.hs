{-# LANGUAGE TemplateHaskell #-}
module Web.Sitemap.Types
  ( URL
  , Entry(..)
  , loc
  , lastmod
  , changefreq
  , priority
  ) where

import Control.Lens
import Data.Text (Text)

type URL = Text

data Entry = Entry
  { _loc :: !URL
  , _lastmod :: Maybe Text
  , _changefreq :: Maybe Text
  , _priority :: Maybe Double
  } deriving (Eq, Show)

makeLenses ''Entry
