# [Lackey][]

[![Build][]](https://travis-ci.org/tfausak/lackey)

Lackey is a Haskell library for generating Ruby consumers of [Servant][] APIs.

-   [Installation](#installation)
-   [Usage](#usage)

## Installation

You can install Lackey by adding it to your Cabal file.

```
build-depends:
    lackey ==0.1.*
```

You can also install it manually.

``` sh
cabal update
cabal install 'lackey ==0.1.*'
```

Please see [the change log][] for a detailed list of changes.

## Usage

Use `Lackey.rubyForAPI` to generate a string of Ruby source code for consuming
a Servant API. For example:

``` hs
type API = "things" :> Get '[JSON] [Thing]

api :: Proxy API
api = Proxy

ruby :: String
ruby = rubyForAPI api
```

[lackey]: https://github.com/tfausak/lackey
[build]: https://img.shields.io/travis/tfausak/lackey.svg
[servant]: http://haskell-servant.github.io
[the change log]: ./CHANGELOG.md
