{-# LANGUAGE FlexibleContexts #-}

{- |
    This module allows you to generate Ruby code for APIs defined by Servant.
-}
module Lackey
    ( rubyForAPI
    ) where

import Data.Proxy (Proxy)
import Lackey.Internal

{- |
    Generate Ruby code from an API description.
-}
rubyForAPI :: (HasCode a, HasRuby (Ruby a)) => Proxy a -> String
rubyForAPI proxy = rubyFor (codeFor proxy defaultEndpoint)
