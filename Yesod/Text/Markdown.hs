-- | In addition to the exports here we also export these instances for 'Markdown'
--
-- 'PersistField', 'PersistFieldSql', 'ToJSON', 'FromJSON'
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Text.Markdown (markdownField) where

import Yesod.Core (RenderMessage)
import Text.Hamlet (hamlet)
import Yesod.Form
import Yesod.Core (HandlerSite)
import Yesod.Core.Widget
import Yesod.Persist
import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)
import Text.Markdown (Markdown (Markdown))
import Database.Persist.Sql
import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Data.Aeson

instance PersistField Markdown where
  toPersistValue (Markdown t) = PersistText $ toStrict t
  fromPersistValue (PersistText t) = Right $ Markdown $ fromStrict t
  fromPersistValue _ = Left "Not a PersistText value"

instance PersistFieldSql Markdown where
    sqlType _ = SqlString

instance ToJSON Markdown where
  toJSON (Markdown text) = object ["markdown" .= text]

instance FromJSON Markdown where
  parseJSON (Object v) = Markdown <$> v .: "markdown"
  parseJSON _ = mzero

-- | Creates a @\<textarea>@ tag whose returned value is wrapped in a
-- 'Markdown' newtype; see 'Markdown' for details.
markdownField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Markdown
markdownField = Field
    { fieldParse = parseHelper $ Right . Markdown . fromStrict
    , fieldView = \theId name attrs val isReq -> toWidget
        [hamlet|$newline never
<textarea id="#{theId}" name="#{name}" :isReq:required="" *{attrs}>#{either id extractStrict val}
|]
   , fieldEnctype = UrlEncoded -- I choose UrlEncoded because textareaField is
     }
     where
        extractStrict :: Markdown -> Text
        extractStrict (Markdown lt) = toStrict lt
