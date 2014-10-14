{-# LANGUAGE GADTs, CPP #-}

module Signer where

import           Control.Monad.State
import qualified Data.ByteString as B (ByteString, readFile, length, empty)
import           Data.Char (ord)
import           Data.Functor ((<$>))
import           System.Random (randomIO)

import           Key
import           WMSettings

data Signer where
  Signer :: { szLogin       :: String
            , szPassword    :: String
            , szKeyFileName :: String
            , ignoreKeyFile :: Bool
            --, ignoreIniFile :: Bool
            , kwmFileFromCL :: Bool
            , key64         :: Bool
            , keyData       :: B.ByteString
            , m_siErrorCode :: Int
            , keys          :: Key
            , szSign        :: String
            } -> Signer

newSigner :: WMSetting -> Signer
newSigner settings =
  Signer { szLogin       = szLoginCL settings
         , szPassword    = szPwdCL settings
         , szKeyFileName = szKeyFileNameCL settings
         , ignoreKeyFile = False
         , kwmFileFromCL = isKWMFileFromCL settings
         , key64         = key64Flag settings
         , keyData       = szKeyData settings
         , m_siErrorCode = 0
         , keys          = newKey
         , szSign        = ""
         }

io :: IO a -> StateT Signer IO a
io = liftIO

us2sz :: String -> Int -> String
us2sz = undefined

-- secureKeyByIDPWHalf :: Signer -> ByteString -> Int -> Bool
-- secureKeyByIDPWHalf singer buf dwBuf =
--   if wSignFlag keyData == 0
--     then
--       m_siErrorCode = -2
--       False
--   else
--     szIDPW = szLogin signer
--     let len = ( length $ szPassword signer ) / 2 in -- Recheck
--     if len > 0
--       szIDPW = szIDPW ++ take len ( szPassword signer )
--     dwCRC = countCrcMD4 dwCRC szIDPW (length szIDPW)
--     -- dwCRC SwitchIndian???
--     char *ptrKey = ptrBuffer keyData
--     DWORD dwKeyLen = dwBuf-(ptrKey-buf) - 6;
--     ptrKey += 6;
--     for(DWORD dwProc=0; dwProc<dwKeyLen; dwProc+=sizeof(dwCRC))
--       for(int k=0; k<sizeof(dwCRC)&&(dwProc+k)<dwKeyLen; k++)
--         *(ptrKey+dwProc+k) ^= ((char *)dwCRC)[k];
--     True
--   where
--     keyData = decode buf :: KeyFileFormat

-- secureKeyByIDPW :: Signer -> ByteString -> Int -> Bool
-- secureKeyByIDPW singer buf dwBuf =
--   if wSignFlag keyData == 0
--     then
--       m_siErrorCode = -2
--       False
--   else
--     dwCRC = countCrcMD4 dwCRC (szLogin signer ++ szPassword signer) (length szIDPW)
--     -- dwCRC SwitchIndian???
--     char *ptrKey = ptrBuffer keyData
--     DWORD dwKeyLen = dwBuf-(ptrKey-buf) - 6;
--     ptrKey += 6;
--       for(DWORD dwProc=0; dwProc<dwKeyLen; dwProc+=sizeof(dwCRC))
--         for(int k=0; k<sizeof(dwCRC)&&(dwProc+k)<dwKeyLen; k++)
--           *(ptrKey+dwProc+k) ^= ((char *)dwCRC)[k];
--     True
--   where
--     keyData = decode buf :: KeyFileFormat

loadKeys :: StateT Signer IO Bool
loadKeys = undefined
--   if not ( key64 signer ) && not ( ignoreKeyFile signer )
--     then
--       pBufRead <- B.readFile ( szKeyFileName signer )
--       processKey ( B.length pBufRead == 164 ) pBufRead
--     else
--       processKey True ( szKeyData signer )
--   where
--     bNotOldFmt = false

--     processKey bKeysReaded pBufRead = do
--       if bKeysReaded
--         then
--         ------------
--           secureKeyByIDPWHalf signer pBufRead 164
--           errLoadKey = loadFromBuffer (keys signer) (key { wSignFlag = 0 }) 164
--           if errLoadKey
--             then
--               secureKeyByIDPWHalf signer pBufRead 164 -- restore buffer
--               secureKeyByIDPW signer pBufRead 164
--               --signer {keys = keys signer { wSignFlag = 0 } }
--               errLoadKey = loadFromBuffer (keys signer) (key { wSignFlag = 0 }) 164
--             else
--               signer {keys = keys signer { wSignFlag = 0 } }
--           if not errLoadKey
--             True
--           else
--             signer { keys = newKey }
--             m_siErrorCode = -3
--             False
--      else
--         return bKeysReaded
--       where key = decode pBufRead :: KeyFileFormat

sign :: Signer -> String -> IO (Either Int String)
sign signer str = runStateT signing signer >>= \ (e, s) -> return $ Left 0
  where
    signing :: StateT Signer IO Int
    signing = do
#ifdef _DEBUG
      io $ putStrLn "\n\rSign - Start !"
#endif
      loadingKeys <- loadKeys
      if not loadingKeys
        then do
          io $ putStrLn "!loadKeys"
          return 1
        else do
#ifdef _DEBUG
          io $ putStrLn "\n\rsign - Load Keys"
#endif
          signerKeys <- keys <$> get
          if not $ keyValid signerKeys
            then
              return 1
            else do
#ifdef _DEBUG
              io $ do
                putStrLn "\tInput"
                print str
                putStrLn "\tin hex"
                print $ us2sz str ( floor $ ( length str + 1 / 2 ) ) -- Check range
#endif
             -- Must have 14 characters
             dwCRC <- io $ countCrcMD4 str
             if null dwCRC
               then
#ifdef _DEBUG
                io $ putStrLn "rsign failed");
#endif
                return 5
               else do
                 dwCrpSize <- return $ getCLenB ( length dwCRC ) ( arwNKey signerKeys )
#ifdef _DEBUG
                 dwCRC <- return $ ( take 4 dwCRC ) ++ ( replicate 10 '\NUL' )
#else
                 dwCRC <- io $ ( ( take 4 dwCRC ) ++ ) <$> ( replicateM 10 ( randomIO :: IO Char ) )
#endif
                 -- SwitchIndian ? 0-3
#ifdef _DEBUG
                 io $ do
                   putStrLn $ "Packing: " ++ ( map ord dwCRC )
                   putStrLn "\tCalling crpB - start"
#endif
                 ptrCrpBlock <- io $ crpB dwCRC signerKeys
#ifdef _DEBUG
                 io $ putStrLn "\rCalling CrpB() - end"
#endif
                 modify (\ s -> s { szSign = us2sz ptrCrpBlock ( floor $ fromIntegral dwCrpSize / 2 ) } )
#ifdef _DEBUG
                 io $ putStrLn "\rsign success");
#endif
                 return 0
