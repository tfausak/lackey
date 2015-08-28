{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lackey.Internal.HasRuby where

import Lackey.Internal.Endpoint
import Lackey.Internal.MatrixItem
import Lackey.Internal.Method
import Lackey.Internal.PathSegment
import Lackey.Internal.QueryItem
import Servant.API ((:>), (:<|>)(..))

import qualified Data.Proxy as Proxy
import qualified GHC.TypeLits as GHC
import qualified Servant.API as Servant

class HasRuby a where
    type Ruby a

    rubyFor :: Proxy.Proxy a -> Endpoint -> Ruby a

instance HasRuby (Servant.Delete a b) where
    type Ruby (Servant.Delete a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Delete
        }

instance HasRuby (Servant.Get a b) where
    type Ruby (Servant.Get a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Get
        }

instance HasRuby (Servant.Patch a b) where
    type Ruby (Servant.Patch a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Patch
        }

instance HasRuby (Servant.Post a b) where
    type Ruby (Servant.Post a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Post
        }

instance HasRuby (Servant.Put a b) where
    type Ruby (Servant.Put a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Put
        }

instance (GHC.KnownSymbol a, HasRuby b) => HasRuby (a :> b) where
    type Ruby (a :> b) = Ruby b

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy b) endpoint
        { endpointPathSegments = endpointPathSegments endpoint ++
            [ PathLiteral (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a))
            ]
        }

instance (HasRuby a, HasRuby b) => HasRuby (a :<|> b) where
    type Ruby (a :<|> b) = Ruby a :<|> Ruby b

    rubyFor _ endpoint
        = rubyFor (Proxy.Proxy :: Proxy.Proxy a) endpoint
        :<|> rubyFor (Proxy.Proxy :: Proxy.Proxy b) endpoint

instance (GHC.KnownSymbol a, HasRuby c) =>
        HasRuby (Servant.Capture a b :> c) where
    type Ruby (Servant.Capture a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointPathSegments = endpointPathSegments endpoint ++
            [ PathCapture (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby b) =>
        HasRuby (Servant.MatrixFlag a :> b) where
    type Ruby (Servant.MatrixFlag a :> b) = Ruby b

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy b) endpoint
        { endpointPathSegments = endpointPathSegments endpoint ++
            [ PathMatrix (MatrixFlag (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a)))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby c) =>
        HasRuby (Servant.MatrixParam a b :> c) where
    type Ruby (Servant.MatrixParam a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointPathSegments = endpointPathSegments endpoint ++
            [ PathMatrix (MatrixParam (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a)))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby c) =>
        HasRuby (Servant.MatrixParams a b :> c) where
    type Ruby (Servant.MatrixParams a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointPathSegments = endpointPathSegments endpoint ++
            [ PathMatrix (MatrixParams (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a)))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby b) =>
        HasRuby (Servant.QueryFlag a :> b) where
    type Ruby (Servant.QueryFlag a :> b) = Ruby b

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy b) endpoint
        { endpointQueryItems = endpointQueryItems endpoint ++
            [ QueryFlag (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby c) =>
        HasRuby (Servant.QueryParam a b :> c) where
    type Ruby (Servant.QueryParam a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointQueryItems = endpointQueryItems endpoint ++
            [ QueryParam (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby c) =>
        HasRuby (Servant.QueryParams a b :> c) where
    type Ruby (Servant.QueryParams a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointQueryItems = endpointQueryItems endpoint ++
            [ QueryParams (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a))
            ]
        }

instance (HasRuby c) => HasRuby (Servant.ReqBody a b :> c) where
    type Ruby (Servant.ReqBody a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointHasBody = True
        }
