# [Lackey][]

[![Version badge][]][version]
[![Build badge][]][build]

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
[Version badge]: https://www.stackage.org/package/lackey/badge/nightly?label=version
[version]: https://www.stackage.org/package/lackey
[Build badge]: https://travis-ci.org/tfausak/lackey.svg?branch=master
[build]: https://travis-ci.org/tfausak/lackey
[Servant]: https://haskell-servant.readthedocs.org/en/stable/
[Excon]: https://rubygems.org/gems/excon
