{-# LANGUAGE GADTs, DeriveGeneric #-}

module Signer where

import           Data.Bits
import qualified Data.Bytestring as B (readFile, length, empty)
import           Data.Word
import           GHC.Generics (Generic)

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
            , szKeyData     :: ByteString
            , m_siErrorCode :: Int
            , keys          :: Key
            } -> Signer

newSigner :: WMSetting -> Signer
newSigner settings =
  Signer { szLogin       = szLoginCL settings
         , szPassword    = szPwdCL settings
         , szKeyFileName = szKeyFileNameCL settings
         , ignoreKeyFile = False
         , kwmFileFromCL = isKWMFileFromCL settings
         , key64         = key64Flag settings
         , szKeyData     = B.empty
         , m_siErrorCode = 0
         , keys          = newKey
         }

data KeyFileFormat where
  KeyFileFormat :: { wReserved1 :: Word16
                   , wSignFlag  :: Word16
                   , dwCRC0     :: Word32
                   , dwCRC1     :: Word32
                   , dwCRC2     :: Word32
                   , dwCRC3     :: Word32
                   , dwLenBuf   :: Word32
                   , ptrBuffer  :: Word8
                   } -> KeyFileFormat deriving (Show, Generic)

instance Binary KeyFileFormat where
  get = do
    reserved <- getWord16host
    sign <- getWord16host
    crc0 <- getWord32host
    crc1 <- getWord32host
    crc2 <- getWord32host
    crc3 <- getWord32host
    lenBuf <- getWord32host
    buf <- getWord8
    return $ KeyFileFormat reserved sign crc0 crc1 crc2 crc3 lenBuf buf

secureKeyByIDPWHalf :: Signer -> ByteString -> Int -> Bool
secureKeyByIDPWHalf singer buf dwBuf =
  if wSignFlag keyData == 0
    then
      m_siErrorCode = -2
      False
  else
    szIDPW = szLogin signer
    let len = ( length $ szPassword signer ) / 2 in -- Recheck
    if len > 0
      szIDPW = szIDPW ++ take len ( szPassword signer )
    dwCRC = countCrcMD4 dwCRC szIDPW (length szIDPW)
    -- dwCRC SwitchIndian???
    char *ptrKey = ptrBuffer keyData
    DWORD dwKeyLen = dwBuf-(ptrKey-buf) - 6;
    ptrKey += 6;
    for(DWORD dwProc=0; dwProc<dwKeyLen; dwProc+=sizeof(dwCRC))
      for(int k=0; k<sizeof(dwCRC)&&(dwProc+k)<dwKeyLen; k++)
        *(ptrKey+dwProc+k) ^= ((char *)dwCRC)[k];
    True
  where
    keyData = decode buf :: KeyFileFormat

secureKeyByIDPW :: Signer -> ByteString -> Int -> Bool
secureKeyByIDPW singer buf dwBuf =
  if wSignFlag keyData == 0
    then
      m_siErrorCode = -2
      False
  else
    dwCRC = countCrcMD4 dwCRC (szLogin signer ++ szPassword signer) (length szIDPW)
    -- dwCRC SwitchIndian???
    char *ptrKey = ptrBuffer keyData
    DWORD dwKeyLen = dwBuf-(ptrKey-buf) - 6;
    ptrKey += 6;
      for(DWORD dwProc=0; dwProc<dwKeyLen; dwProc+=sizeof(dwCRC))
        for(int k=0; k<sizeof(dwCRC)&&(dwProc+k)<dwKeyLen; k++)
          *(ptrKey+dwProc+k) ^= ((char *)dwCRC)[k];
    True
  where
    keyData = decode buf :: KeyFileFormat

loadKeys :: Signer -> IO Signer
loadKeys signer =	do
  if not ( key64 signer ) && not ( ignoreKeyFile signer )
    then
      pBufRead <- B.readFile ( szKeyFileName signer )
      processKey ( B.length pBufRead == 164 ) pBufRead
    else
      processKey True ( szKeyData signer )
  where
    bNotOldFmt = false

    processKey bKeysReaded pBufRead = do
      if bKeysReaded
        then
        ------------
          secureKeyByIDPWHalf signer pBufRead 164
          errLoadKey = loadFromBuffer (keys signer) (key { wSignFlag = 0 }) 164
          if errLoadKey
            then
              secureKeyByIDPWHalf signer pBufRead 164 -- restore buffer
              secureKeyByIDPW signer pBufRead 164
              --signer {keys = keys signer { wSignFlag = 0 } }
              errLoadKey = loadFromBuffer (keys signer) (key { wSignFlag = 0 }) 164
            else
              signer {keys = keys signer { wSignFlag = 0 } }
          if not errLoadKey
            True
          else
            signer { keys = newKey }
            m_siErrorCode = -3
            False
     else
        return bKeysReaded
      where key = decode pBufRead :: KeyFileFormat

sign :: Signer -> String -> IO (Either Int String)
sign signer str = do
  putStrLn "\n\rSign - Start !"
  signer' <- return $ loadKeys signer
