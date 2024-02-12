{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Encoins.Common.Version
  (
    AppVersion(..)
  , appVersion
  , showAppVersion
  ) where

import           Data.Aeson                    (FromJSON (..), ToJSON (..))
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text, pack)
import qualified Data.Text                     as T
import           Data.Time                     (UTCTime, defaultTimeLocale,
                                                formatTime, parseTimeM)
import           Data.Time.Clock.POSIX         (posixSecondsToUTCTime)
import           Data.Version                  (Version, showVersion)
import           Development.GitRev            (gitCommitDate, gitHash)
import           Encoins.Common.Constant       (space, column)
import           GHC.Generics                  (Generic)
import           Prettyprinter                 (Pretty (pretty), annotate,
                                                defaultLayoutOptions,
                                                layoutSmart)
import           Prettyprinter.Render.Terminal (Color (Blue, Green), bold,
                                                color, renderStrict)

data AppVersion = MkAppVersion
  { avVersion :: Version
  , avCommit  :: Text
  , avDate    :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

appVersion :: Version -> AppVersion
appVersion version = MkAppVersion
  { avVersion = version
  , avCommit = $(gitHash)
  , avDate = fromMaybe (posixSecondsToUTCTime $ toEnum 0) $ parseTimeM
      False
      defaultTimeLocale
      "%a %b %e %T %Y %Z"
      $(gitCommitDate)
  }

showAppVersion :: Text -> AppVersion -> Text
showAppVersion appName sv = T.intercalate "\n" $ [sVersion, sHash, sDate]
    where
        sVersion = textToColorText green $ appName <> space <> "v" <> T.pack (showVersion $ avVersion sv)
        sHash = space <> "➤" <> space <> (textToColorText blue ("Git revision" <> column <> space :: T.Text)) <> avCommit sv
        sDate = space <> "➤" <> space <> (textToColorText blue ("Commit date" <> column <> space :: T.Text)) <> dateFormated
        dateFormated = formatPollTime $ avDate sv
        textToColorText col txt = renderStrict $ layoutSmart defaultLayoutOptions $ col $ pretty txt
        green = annotate $ color Green <> bold
        blue = annotate $ color Blue <> bold

formatPollTime :: UTCTime -> Text
formatPollTime
  = pack
  . formatTime defaultTimeLocale "%e %B %Y, %R %Z"
