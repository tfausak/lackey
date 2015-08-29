{-# LANGUAGE FlexibleContexts #-}

{- |
    This module is for internal use only and should be considered private. If
    you need anything exported from this module, please contact the maintainer.
-}
module Lackey.Internal where

import Data.Proxy (Proxy)
import Lackey.Internal.Endpoint (defaultEndpoint)
import Lackey.Internal.HasCode (HasCode, codeFor)
import Lackey.Internal.HasRuby (HasRuby, Ruby, rubyFor)

ruby :: (HasRuby a) => Proxy a -> Ruby a
ruby proxy = rubyFor proxy defaultEndpoint

{- |
    Generate Ruby code from an API description.
-}
rubyForAPI :: (HasRuby a, HasCode (Ruby a)) => Proxy a -> String
rubyForAPI proxy = codeFor (ruby proxy)
