{-# LANGUAGE GADTs #-}

module WMSettings where

import qualified Data.ByteString.Lazy.Internal as B (ByteString)
import qualified Data.ByteString.Lazy as B (ByteString, pack, empty, length)

data WMSetting where
  WMSetting :: { szLoginCL        :: String
               , szPwdCL          :: String
               , szIniFileFull    :: String
               , szKeyFileNameCL  :: String
               , szKeyData        :: B.ByteString
               , szStringToSign   :: String
               , key64Flag        :: Bool
               , isKWMFileFromCL  :: Bool } -> WMSetting
  deriving Show

newSettings :: WMSetting
newSettings = WMSetting "" "" "" "" B.empty "" False False
