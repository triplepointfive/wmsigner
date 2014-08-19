{-# LANGUAGE GADTs #-}

module Main where

import Paths_WMSigner (version)
import Data.Version (showVersion)

import System.Environment (getProgName, getArgs)
import System.Exit(exitSuccess, exitFailure)
import Data.Functor ((<$>))

fatalError :: String -> IO a
fatalError errorMessage = putStrLn errorMessage >> exitFailure

data WMSetting where
  WMSetting :: { szLoginCL :: String, szPwdCL :: String, szFileNameCL :: String
                , szKeyFileNameCL :: String, szKeyData :: String, szStringToSign :: String, key64Flag :: Bool, isKWMFileFromCL :: Bool } -> WMSetting
  deriving Show

commandLineParse :: [String] -> IO WMSetting
commandLineParse args = processArgument args ( WMSetting "" "" "" "" "" "" False False )
  where
    processArgument :: [String] -> WMSetting -> IO WMSetting
    processArgument [] settings = return settings
    processArgument (arg:rest) settings
      | arg `elem` ["-h", "--help"] = do
        putStrLn $ "WMSigner, Version " ++ ( showVersion version ) ++ ", 2014"
        putStrLn " -p   [--password]   : Password for key_file"
        putStrLn " -w   [--wmid]       : 123456789012 : WMID (12 digits)"
        putStrLn " -s   [--sign]       : string_to_signification : signing specified string"
        putStrLn " -i   [--ini-path]   : Correct path to ini_file with ini_file_name *.ini"
        putStrLn " -k   [--key-path]   : Correct path to key_file with key_file_name"
        putStrLn " -K64 [--key-base64] : Text string in Base64 code, contains the key for wmsigner"
        putStrLn " -h   [--help]       : Help (this srceen)"
        putStrLn " -v   [--version]    : Version of program"
        exitSuccess
      | arg `elem` ["-v", "--version"] = do
        putStrLn $ "WMSigner, Version " ++ ( showVersion version ) ++ ", 2014"
        exitSuccess
      | arg `elem` ["-p", "--password"] = if null rest then fatalError "Password not defined!" else next settings { szPwdCL = value }
      | arg `elem` ["-w", "--wmid"] = if null rest then fatalError "WMID Not defined!" else next settings { szLoginCL = value }
      | arg `elem` ["-s", "--sign"] = if null rest then fatalError "String to signification not defined!" else next settings { szStringToSign = value }
      | arg `elem` ["-i", "--ini-path"] = if null rest then fatalError "Ini file name (with path) not defined!" else next settings { szFileNameCL = value }
      | arg `elem` ["-k", "--key-path"] = if null rest then fatalError "Key file not defined!" else next settings { szKeyFileNameCL = value, isKWMFileFromCL = True }
      | arg `elem` ["-K64", "--key-base64"] = do
        if null rest then fatalError "KEY_STRING in Base64 code not defined!"
          else 
            if length value != 220 then fatalError "Key string has illegal length!"
              else let bytes = code64( ENCODE, KeyBuffer, 512, szKeyData, 220 ); in
                if length bytes != 164 then fatalError "Bad key string in parameter!"
                  else next setting { key64Flag = True, szKeyData = KeyBuffer } -- 164
      | otherwise = fatalError "Illegal command line option found! Use option --help or -h for information."
      where
        next = processArgument ( tail rest )
        value = head rest

main :: IO ()
main = do
  szIniFileFull <- (++ ".ini") <$> getProgName
  args <- getArgs
  if null args then
    print "Null found"
  else do
    settings <- commandLineParse args
    print settings
