{-# LANGUAGE GADTs #-}

module Main where

-- import           Paths_WMSigner (version)
import           Data.Either (either)
import           Data.Functor ((<$>))
import           Data.Version (showVersion)
import           System.Directory (doesFileExist)
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitSuccess, exitFailure)

import           Data.ByteString.Base64.Lazy (decode)
import qualified Data.ByteString.Lazy.Internal as B (ByteString)
import qualified Data.ByteString.Lazy as B (length, empty)
import qualified Data.ByteString.Lazy.Char8 as B (pack)

import           Signer
import           Utils
import           WMSettings

fatalError :: String -> IO a
fatalError errorMessage = putStrLn errorMessage >> exitFailure

printError :: Int -> IO a
printError errorCode = fatalError ( "Error " ++ show errorCode )

commandLineParse :: [String] -> IO WMSetting
commandLineParse args = processArgument args newSettings
  where
    processArgument :: [String] -> WMSetting -> IO WMSetting
    processArgument [] settings = return settings
    processArgument (arg:rest) settings
      | arg `elem` ["-h", "--help"] = do
        --putStrLn $ "WMSigner, Version " ++ ( showVersion version ) ++ ", 2014\n"
        putStrLn " -p   [--password]   : Password for key_file"
        putStrLn " -w   [--wmid]       : 123456789012 : WMID (12 digits)"
        putStrLn " -s   [--sign]       : string_to_signification : signing specified string"
        putStrLn " -i   [--ini-path]   : Correct path to ini_file with ini_file_name *.ini"
        putStrLn " -k   [--key-path]   : Correct path to key_file with key_file_name"
        putStrLn " -K64 [--key-base64] : Text string in Base64 code, contains the key for wmsigner"
        putStrLn " -h   [--help]       : Help (this srceen)"
        putStrLn " -v   [--version]    : Version of program\n"
        exitSuccess
      -- | arg `elem` ["-v", "--version"] = putStrLn ( "WMSigner, Version " ++ ( showVersion version ) ++ ", 2014" ) >> exitSuccess
      | arg `elem` ["-p", "--password"] = if null rest then fatalError "Password not defined!" else next settings { szPwdCL = value }
      | arg `elem` ["-w", "--wmid"] = if null rest then fatalError "WMID Not defined!" else next settings { szLoginCL = value }
      | arg `elem` ["-s", "--sign"] = if null rest then fatalError "String to signification not defined!" else next settings { szStringToSign = value }
      | arg `elem` ["-i", "--ini-path"] = if null rest then fatalError "Ini file name (with path) not defined!" else next settings { szIniFileFull = value }
      | arg `elem` ["-k", "--key-path"] = if null rest then fatalError "Key file not defined!" else next settings { szKeyFileNameCL = value, isKWMFileFromCL = True }
      | arg `elem` ["-K64", "--key-base64"] =
        if null rest then fatalError "KEY_STRING in Base64 code not defined!"
          else
            if length value /= 220 then fatalError "Key string has illegal length!"
              else either fatalError
                (\ bytes  -> if B.length bytes /= 164 then fatalError "Bad key string in parameter!" else next settings { key64Flag = True, szKeyData = bytes } )
                ( decode ( B.pack value ) )
      | otherwise = fatalError "Illegal command line option found! Use option --help or -h for information."
      where
        next = processArgument ( tail rest )
        value = head rest

fullSettings :: WMSetting -> Bool
fullSettings settings = ( key64Flag settings || isKWMFileFromCL settings ) && not ( null $ szLoginCL settings ) && not ( null $ szLoginCL settings )

loadIniFile :: FilePath -> WMSetting -> IO WMSetting
loadIniFile fName settings =
  let
    process True = ( lines <$> readFile fName ) >>= validateFile
    process False = printError 20
  in doesFileExist fName >>= process
  where
    validateFile :: [String] -> IO WMSetting
    validateFile [] = printError (-4)
    validateFile ("":_) = printError (-4)
    validateFile (_:[]) = printError (-5)
    validateFile (_:"":[]) = printError (-5)
    validateFile (_:_:[]) = printError (-6)
    validateFile (_:_:"":[]) = printError (-6)
    validateFile (login:pwd:fileName:_) =
      return $ settings
        { szLoginCL       = null ( szLoginCL settings ) ? login :? szLoginCL settings
        , szPwdCL         = null ( szPwdCL settings ) ? pwd :? szPwdCL settings
        , szKeyFileNameCL = isKWMFileFromCL settings ? szKeyFileNameCL settings :? fileName
        }

normStr :: String -> String
normStr [] = []
normStr (a:[]) = if a `elem` "\EOT\n\r" then [] else [a]
normStr str = takeWhile (`notElem` "\NUL\EOT") $ filter (/='\r') $ reverse $
  ( a `elem` "\EOT\n\r" ? '\NUL' :? a ) : ( b `elem` "\EOT\n\r" ? '\NUL' :? b ) :  c
  where (a:b:c) = reverse str

readConsoleString :: IO String
readConsoleString = reverse <$> go ""
  where
    go contents = getChar >>= process
      where
        process c
          | c == '\r' = go contents
          | c `elem` "\EOT\NUL" = go contents
          | otherwise = go ( c:contents )

main :: IO ()
main = do
  szIniFileFull <- (++ ".ini") <$> getProgName
  args <- getArgs
  if null args then
    print "Null found"
  else do
    settings <- commandLineParse args
    if fullSettings settings then
      signForSettings settings
    else
      loadIniFile szIniFileFull settings >>= signForSettings
  where
    signForSettings :: WMSetting -> IO ()
    signForSettings settings = ( if null $ szStringToSign settings
      then
        ( normStr <$> readConsoleString ) >>= sign ( newSigner settings )
      else
        sign ( newSigner settings ) ( normStr $ szStringToSign settings )
      ) >>= either printError putStrLn
