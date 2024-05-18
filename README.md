# [Lackey][]

[![Workflow](https://github.com/tfausak/lackey/actions/workflows/ci.yml/badge.svg)](https://github.com/tfausak/lackey/actions/workflows/ci.yml)
[![Hackage](https://badgen.net/hackage/v/lackey)](https://hackage.haskell.org/package/lackey)

Lackey is a Haskell library for generating Ruby consumers of [Servant][] APIs.

Use `Lackey.rubyForAPI` to generate a string of Ruby source code for consuming
a Servant API. For example:

``` hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Lackey
import Servant.API

type API = "words" :> Get '[JSON] [String]

api :: Proxy.Proxy API
api = Proxy.Proxy

ruby :: Text.Text
ruby = Lackey.rubyForAPI api

main :: IO ()
main = Text.putStrLn ruby
-- def get_words(excon)excon.request(:method=>:get,:path=>"/words",:headers=>{},:body=>nil)end
```

The generated functions require [Excon][].

[Lackey]: https://github.com/tfausak/lackey
[Servant]: https://haskell-servant.readthedocs.org/en/stable/
[Excon]: https://rubygems.org/gems/excon
