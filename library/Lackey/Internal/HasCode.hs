{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lackey.Internal.HasCode where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lackey.Internal.Endpoint
import Lackey.Internal.Header
import Lackey.Internal.MatrixItem
import Lackey.Internal.Method
import Lackey.Internal.PathSegment
import Lackey.Internal.QueryItem
import Servant.API ((:>), (:<|>)((:<|>)))

import qualified Servant.API as S

class HasCode a where
    type Ruby a
    codeFor :: Proxy a -> Endpoint -> Ruby a

instance HasCode S.Raw where
    type Ruby S.Raw = ()
    codeFor _ _ = ()

instance (HasCode a, HasCode b) => HasCode (a :<|> b) where
    type Ruby (a :<|> b) = Ruby a :<|> Ruby b
    codeFor _ e = codeFor a e :<|> codeFor b e where
        a = Proxy :: Proxy a
        b = Proxy :: Proxy b

instance (HasCode c) => HasCode (S.ReqBody a b :> c) where
    type Ruby (S.ReqBody a b :> c) = Ruby c
    codeFor _ e = codeFor c (e { endpointHasBody = True}) where
        c = Proxy :: Proxy c

-- Methods

instance HasCode (S.Delete a b) where
    type Ruby (S.Delete a b) = Endpoint
    codeFor _ e = e { endpointMethod = Delete }

instance HasCode (S.Get a b) where
    type Ruby (S.Get a b) = Endpoint
    codeFor _ e = e { endpointMethod = Get }

instance HasCode (S.Patch a b) where
    type Ruby (S.Patch a b) = Endpoint
    codeFor _ e = e { endpointMethod = Patch }

instance HasCode (S.Post a b) where
    type Ruby (S.Post a b) = Endpoint
    codeFor _ e = e { endpointMethod = Post }

instance HasCode (S.Put a b) where
    type Ruby (S.Put a b) = Endpoint
    codeFor _ e = e { endpointMethod = Put }

-- Path segments

instance (KnownSymbol symbol, HasCode a) => HasCode (symbol :> a) where
    type Ruby (symbol :> a) = Ruby a
    codeFor _ e = codeFor a (e { endpointPathSegments = segments }) where
        a = Proxy :: Proxy a
        segments = endpointPathSegments e ++ [segment]
        segment = PathLiteral (symbolVal symbol)
        symbol = Proxy :: Proxy symbol

instance (KnownSymbol symbol, HasCode b) => HasCode (S.Capture symbol a :> b) where
    type Ruby (S.Capture symbol a :> b) = Ruby b
    codeFor _ e = codeFor b (e { endpointPathSegments = segments }) where
        b = Proxy :: Proxy b
        segments = endpointPathSegments e ++ [segment]
        segment = PathCapture (symbolVal symbol)
        symbol = Proxy :: Proxy symbol

-- Matrix items

instance (KnownSymbol symbol, HasCode a) => HasCode (S.MatrixFlag symbol :> a) where
    type Ruby (S.MatrixFlag symbol :> a) = Ruby a
    codeFor _ e = codeFor a (e { endpointPathSegments = segments }) where
        a = Proxy :: Proxy a
        segments = endpointPathSegments e ++ [segment]
        segment = PathMatrix (MatrixFlag (symbolVal symbol))
        symbol = Proxy :: Proxy symbol

instance (KnownSymbol symbol, HasCode b) => HasCode (S.MatrixParam symbol a :> b) where
    type Ruby (S.MatrixParam symbol a :> b) = Ruby b
    codeFor _ e = codeFor b (e { endpointPathSegments = segments }) where
        b = Proxy :: Proxy b
        segments = endpointPathSegments e ++ [segment]
        segment = PathMatrix (MatrixParam (symbolVal symbol))
        symbol = Proxy :: Proxy symbol

instance (KnownSymbol symbol, HasCode b) => HasCode (S.MatrixParams symbol a :> b) where
    type Ruby (S.MatrixParams symbol a :> b) = Ruby b
    codeFor _ e = codeFor b (e { endpointPathSegments = segments }) where
        b = Proxy :: Proxy b
        segments = endpointPathSegments e ++ [segment]
        segment = PathMatrix (MatrixParams (symbolVal symbol))
        symbol = Proxy :: Proxy symbol

-- Query items

instance (KnownSymbol symbol, HasCode a) => HasCode (S.QueryFlag symbol :> a) where
    type Ruby (S.QueryFlag symbol :> a) = Ruby a
    codeFor _ e = codeFor a (e { endpointQueryItems = items }) where
        a = Proxy :: Proxy a
        items = endpointQueryItems e ++ [item]
        item = QueryFlag (symbolVal symbol)
        symbol = Proxy :: Proxy symbol

instance (KnownSymbol symbol, HasCode b) => HasCode (S.QueryParam symbol a :> b) where
    type Ruby (S.QueryParam symbol a :> b) = Ruby b
    codeFor _ e = codeFor b (e { endpointQueryItems = items }) where
        b = Proxy :: Proxy b
        items = endpointQueryItems e ++ [item]
        item = QueryParam (symbolVal symbol)
        symbol = Proxy :: Proxy symbol

instance (KnownSymbol symbol, HasCode b) => HasCode (S.QueryParams symbol a :> b) where
    type Ruby (S.QueryParams symbol a :> b) = Ruby b
    codeFor _ e = codeFor b (e { endpointQueryItems = items }) where
        b = Proxy :: Proxy b
        items = endpointQueryItems e ++ [item]
        item = QueryParams (symbolVal symbol)
        symbol = Proxy :: Proxy symbol

-- Headers

instance (KnownSymbol symbol, HasCode b) => HasCode (S.Header symbol a :> b) where
    type Ruby (S.Header symbol a :> b) = Ruby b
    codeFor  _ e = codeFor b (e { endpointHeaders = headers }) where
        b = Proxy :: Proxy b
        headers = endpointHeaders e ++ [header]
        header = Header (symbolVal symbol)
        symbol = Proxy :: Proxy symbol
