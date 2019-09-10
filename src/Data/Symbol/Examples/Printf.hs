{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Data.Symbol.Examples.Printf where

import Data.Symbol.Utils
import GHC.TypeLits
import Data.Proxy
import Data.Monoid

test :: String
test = printf @"Wurble %d %d %s" 10 20 "foo"

data Specifier
  = D
  | S
  | Lit Symbol

class FormatF (format :: [Specifier]) fun | format -> fun where
  formatF :: String -> fun

instance FormatF '[] String where
  formatF = id

instance FormatF rest fun => FormatF (D ': rest) (Int -> fun) where
  formatF str i
    = formatF @rest (str Data.Monoid.<> show i)

instance FormatF rest fun => FormatF (S ': rest) (String -> fun) where
  formatF str s
    = formatF @rest (str <> s)

instance (FormatF rest fun, KnownSymbol l) => FormatF (Lit l ': rest) fun where
  formatF str
    = formatF @rest (str <> symbolVal (Proxy @l))

class PrintF (sym :: Symbol) fun where
  printf :: fun

type family Parse (lst :: [Symbol]) :: [Specifier] where
  Parse '[] = '[Lit ""]
  Parse ("%" ': "d" ': xs) = D ': Parse xs
  Parse ("%" ': "s" ': xs) = S ': Parse xs
  Parse (x ': xs) = Parse2 x (Parse xs)

type family Parse2 (c :: Symbol) (lst :: [Specifier]) :: [Specifier] where
  Parse2 c ('Lit s ': ss) = 'Lit (AppendSymbol c s) ': ss
  Parse2 c ss = 'Lit c ': ss

instance (Listify sym lst, fmt ~ Parse lst, FormatF fmt fun) => PrintF (sym :: Symbol) fun where
  printf = formatF @fmt ""
