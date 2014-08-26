{-# LANGUAGE GADTs #-}

module WMSettings where

import qualified Data.ByteString.Char8 as BS (pack, empty, length, ByteString)

data WMSetting where
  WMSetting :: { szLoginCL        :: String
               , szPwdCL          :: String
               , szIniFileFull    :: String
               , szKeyFileNameCL  :: String
               , szKeyData        :: BS.ByteString
               , szStringToSign   :: String
               , key64Flag        :: Bool
               , isKWMFileFromCL  :: Bool } -> WMSetting
  deriving Show

newSettings :: WMSetting
newSettings = WMSetting "" "" "" "" BS.empty "" False False