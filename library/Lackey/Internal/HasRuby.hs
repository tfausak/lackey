{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lackey.Internal.HasRuby where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lackey.Internal.Endpoint
import Lackey.Internal.MatrixItem
import Lackey.Internal.Method
import Lackey.Internal.PathSegment
import Lackey.Internal.QueryItem
import Servant.API ((:>), (:<|>)((:<|>)))

import qualified Servant.API as S

class HasRuby a where
    type Ruby a
    rubyFor :: Proxy a -> Endpoint -> Ruby a

instance (HasRuby a, HasRuby b) => HasRuby (a :<|> b) where
    type Ruby (a :<|> b) = Ruby a :<|> Ruby b
    rubyFor _ e = rubyFor a e :<|> rubyFor b e where
        a = Proxy :: Proxy a
        b = Proxy :: Proxy b

instance (HasRuby c) => HasRuby (S.ReqBody a b :> c) where
    type Ruby (S.ReqBody a b :> c) = Ruby c
    rubyFor _ e = rubyFor c (e { endpointHasBody = True}) where
        c = Proxy :: Proxy c

-- Methods

instance HasRuby (S.Delete a b) where
    type Ruby (S.Delete a b) = Endpoint
    rubyFor _ e = e { endpointMethod = Delete }

instance HasRuby (S.Get a b) where
    type Ruby (S.Get a b) = Endpoint
    rubyFor _ e = e { endpointMethod = Get }

instance HasRuby (S.Patch a b) where
    type Ruby (S.Patch a b) = Endpoint
    rubyFor _ e = e { endpointMethod = Patch }

instance HasRuby (S.Post a b) where
    type Ruby (S.Post a b) = Endpoint
    rubyFor _ e = e { endpointMethod = Post }

instance HasRuby (S.Put a b) where
    type Ruby (S.Put a b) = Endpoint
    rubyFor _ e = e { endpointMethod = Put }

-- Path segments

instance (KnownSymbol symbol, HasRuby a) => HasRuby (symbol :> a) where
    type Ruby (symbol :> a) = Ruby a
    rubyFor _ e = rubyFor a (e { endpointPathSegments = segments }) where
        a = Proxy :: Proxy a
        segments = endpointPathSegments e ++ [segment]
        segment = PathLiteral (symbolVal symbol)
        symbol = Proxy :: Proxy symbol

instance (KnownSymbol symbol, HasRuby b) => HasRuby (S.Capture symbol a :> b) where
    type Ruby (S.Capture symbol a :> b) = Ruby b
    rubyFor _ e = rubyFor b (e { endpointPathSegments = segments }) where
        b = Proxy :: Proxy b
        segments = endpointPathSegments e ++ [segment]
        segment = PathCapture (symbolVal symbol)
        symbol = Proxy :: Proxy symbol

-- Matrix items

instance (KnownSymbol symbol, HasRuby a) => HasRuby (S.MatrixFlag symbol :> a) where
    type Ruby (S.MatrixFlag symbol :> a) = Ruby a
    rubyFor _ e = rubyFor a (e { endpointPathSegments = segments }) where
        a = Proxy :: Proxy a
        segments = endpointPathSegments e ++ [segment]
        segment = PathMatrix (MatrixFlag (symbolVal symbol))
        symbol = Proxy :: Proxy symbol

instance (KnownSymbol symbol, HasRuby b) => HasRuby (S.MatrixParam symbol a :> b) where
    type Ruby (S.MatrixParam symbol a :> b) = Ruby b
    rubyFor _ e = rubyFor b (e { endpointPathSegments = segments }) where
        b = Proxy :: Proxy b
        segments = endpointPathSegments e ++ [segment]
        segment = PathMatrix (MatrixParam (symbolVal symbol))
        symbol = Proxy :: Proxy symbol

instance (KnownSymbol symbol, HasRuby b) => HasRuby (S.MatrixParams symbol a :> b) where
    type Ruby (S.MatrixParams symbol a :> b) = Ruby b
    rubyFor _ e = rubyFor b (e { endpointPathSegments = segments }) where
        b = Proxy :: Proxy b
        segments = endpointPathSegments e ++ [segment]
        segment = PathMatrix (MatrixParams (symbolVal symbol))
        symbol = Proxy :: Proxy symbol

-- Query items

instance (KnownSymbol symbol, HasRuby a) => HasRuby (S.QueryFlag symbol :> a) where
    type Ruby (S.QueryFlag symbol :> a) = Ruby a
    rubyFor _ e = rubyFor a (e { endpointQueryItems = items }) where
        a = Proxy :: Proxy a
        items = endpointQueryItems e ++ [item]
        item = QueryFlag (symbolVal symbol)
        symbol = Proxy :: Proxy symbol

instance (KnownSymbol symbol, HasRuby b) => HasRuby (S.QueryParam symbol a :> b) where
    type Ruby (S.QueryParam symbol a :> b) = Ruby b
    rubyFor _ e = rubyFor b (e { endpointQueryItems = items }) where
        b = Proxy :: Proxy b
        items = endpointQueryItems e ++ [item]
        item = QueryParam (symbolVal symbol)
        symbol = Proxy :: Proxy symbol

instance (KnownSymbol symbol, HasRuby b) => HasRuby (S.QueryParams symbol a :> b) where
    type Ruby (S.QueryParams symbol a :> b) = Ruby b
    rubyFor _ e = rubyFor b (e { endpointQueryItems = items }) where
        b = Proxy :: Proxy b
        items = endpointQueryItems e ++ [item]
        item = QueryParams (symbolVal symbol)
        symbol = Proxy :: Proxy symbol
