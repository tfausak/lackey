{-# LANGUAGE FlexibleContexts #-}

{- |
    This module is for internal use only and should be considered private. If
    you need anything exported from this module, please contact the maintainer.
-}
module Lackey.Internal where

import Data.Proxy (Proxy)
import Lackey.Internal.Endpoint (defaultEndpoint)
import Lackey.Internal.HasCode (HasCode, Ruby, codeFor)
import Lackey.Internal.HasRuby (HasRuby, rubyFor)

ruby :: (HasCode a) => Proxy a -> Ruby a
ruby proxy = codeFor proxy defaultEndpoint

{- |
    Generate Ruby code from an API description.
-}
rubyForAPI :: (HasCode a, HasRuby (Ruby a)) => Proxy a -> String
rubyForAPI proxy = rubyFor (ruby proxy)
