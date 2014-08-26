{-# LANGUAGE GADTs #-}

module Signer where

import           WMSettings

data Signer where
  Signer :: { szLogin :: String
            , szPassword :: String
            , szKeyFileName :: String
            , ignoreKeyFile :: Bool
            --, ignoreIniFile :: Bool
            , kwmFileFromCL :: Bool
            , key64 :: Bool
            } -> Signer

sign :: Signer -> String -> IO (Either Int String)
sign = undefined

newSigner :: WMSetting -> Signer
newSigner settings = 
  Signer { szLogin = szLoginCL settings
         , szPassword = szPwdCL settings
         , szKeyFileName = szKeyFileNameCL settings
         , ignoreKeyFile = False
         , kwmFileFromCL = isKWMFileFromCL settings
         , key64 = key64Flag settings
         }