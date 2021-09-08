{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}

-- | Closed type classes are defined by adding a private field with a default
-- implementation that requires a 'Closed' constraint. For example:
--
-- > {-# LANGUAGE DefaultSignatures, FlexibleContexts #-}
-- >
-- > module Foo (Foo) where
-- >
-- > class Foo a where
-- >   default _private :: Closed Foo => Proxy a
-- >   _private :: Proxy a
-- >   _private = Proxy
-- >
-- > instance Foo Int where
-- >   _private = Proxy
--
-- Since @_private@ is not exported, any new instance defined outside the @Foo@
-- module will be forced to use the default implementation, which has the
-- unsatisfiable 'Closed' @Foo@ constraint.
--
-- > module Bar where
-- >
-- > import Foo
-- >
-- > instance Foo Char -- • User defined Foo instances are not allowed.
--
-- Thus new instances can only be defined in the same module as @Foo@, where
-- @_private@ is visible. Note that 'Closed' itself is a closed type class,
-- implemented using the same trick!
--
-- To automatically generate the boilerplate, see "Data.Class.Closed.TH".
module Data.Class.Closed (Closed) where

import Data.Proxy   (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- | 'Closed' constraints are unsatisfiable. Attempting to resolve a 'Closed'
-- @\<class\>@ constraint will result in
--
-- > • User defined <class> instances are not allowed.
class Closed (a ∷ k) where
  default _private ∷ Closed (Closed @k) ⇒ Proxy a
  _private ∷ Proxy a
  _private = Proxy

instance TypeError ('Text "User defined " ':<>: 'ShowType a ':<>: 'Text " instances are not allowed.") ⇒ Closed a where
  _private = Proxy
