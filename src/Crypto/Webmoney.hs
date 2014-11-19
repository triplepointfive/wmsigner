module Crypto.Webmoney
    (
      module Crypto.Internal.Signer
    )
    where

import qualified Crypto.Internal.Signer as S (Signer, newSigner, sign,
                                              signUnsafe)
