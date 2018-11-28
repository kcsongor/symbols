{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Symbol.Utils
 ( Uncons (..)
 , Listify (..)
 ) where

import Data.Symbol.Ascii

import Data.Proxy
import GHC.TypeLits

class Uncons (sym :: Symbol) (h :: Symbol) (t :: Symbol) where
  uncons :: Proxy '(h, t)

instance (h ~ Head sym, AppendSymbol h t ~ sym) => Uncons sym h t where
  uncons = Proxy

class Listify (sym :: Symbol) (result :: [Symbol]) where
  listify :: Proxy result

instance {-# OVERLAPPING #-} nil ~ '[] => Listify "" nil where
  listify = Proxy

instance (Uncons sym h t, Listify t result, result' ~ (h ': result)) => Listify sym result' where
  listify = Proxy
