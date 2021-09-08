{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Data.Class.Closed.Example (
  -- Explicit export is required or else the private field will be exported.
  Example₁ (example₁),
  Example₂ (example₂)
) where

import Data.Class.Closed.TH (close)
import Data.Proxy           (Proxy (..))

close [d|
  class Example₁ a where
    example₁ ∷ a
  instance Example₁ () where
    example₁ = ()
  class Example₂ a b where
    example₂ ∷ a → Proxy (b ∷ Bool)
  instance Example₂ a 'True where
    example₂ _ = Proxy
  |]

-- The following does not work unless it is moved inside the quotation.
-- instance Example₁ Int where
--   example₁ = 0
