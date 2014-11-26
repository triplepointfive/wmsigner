module Crypto.Webmoney
    (
      newSigner
    , sign
    , signUnsafe
    , S.Signer
    )
    where

import qualified Crypto.Internal.Signer as S (Signer, newSigner, sign,
                                              signUnsafe)

-- |Initializes new 'Signer' object, takes exponent and modulus as arguments
newSigner = S.newSigner

-- |Calculates static signature for string
signUnsafe = S.signUnsafe

-- |Calculates randomized signature for string
sign = S.sign