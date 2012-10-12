{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Text.Markdown where

import Yesod.Core (RenderMessage)
import Text.Hamlet (hamlet)
import Yesod.Form
import Yesod.Widget (toWidget)
import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)
import Text.Markdown (Markdown (Markdown))
import Database.Persist ()
import Database.Persist.Store

instance PersistField Markdown where
  toPersistValue (Markdown t) = PersistText $ toStrict t
  fromPersistValue (PersistText t) = Right $ Markdown $ fromStrict t
  fromPersistValue _ = Left "Not a PersistText value"
  sqlType _ = SqlString
  isNullable _ = False

instance ToField Markdown master where
    toField = areq markdownField

instance ToField (Maybe Markdown) master where
    toField = aopt markdownField

markdownField :: RenderMessage master FormMessage => Field sub master Markdown
markdownField = Field
    { fieldParse = parseHelper $ Right . Markdown . fromStrict
    , fieldView = \theId name attrs val _isReq -> toWidget
        [hamlet|$newline never
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id extractStrict val}
|]
     }
     where
        extractStrict :: Markdown -> Text
        extractStrict (Markdown lt) = toStrict lt

