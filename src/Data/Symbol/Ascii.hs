{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

module Data.Symbol.Ascii
  (
    Head
  , ToList
  , ToUpper
  , ToLower
  , ReadNat
  ) where

import GHC.TypeLits
import Data.Symbol.Ascii.Internal (Head, ToList)

--------------------------------------------------------------------------------

-- | Convert the symbol to uppercase
type family ToUpper (sym :: Symbol) :: Symbol where
  ToUpper sym = ToUpper1 (ToList sym)

type family ToUpper1 (sym :: [Symbol]) :: Symbol where
  ToUpper1 '[] = ""
  ToUpper1 (x ': xs) = AppendSymbol (ToUpperC x) (ToUpper1 xs)

type family ToUpperC (sym :: Symbol) :: Symbol where
  ToUpperC "a" = "A"
  ToUpperC "b" = "B"
  ToUpperC "c" = "C"
  ToUpperC "d" = "D"
  ToUpperC "e" = "E"
  ToUpperC "f" = "F"
  ToUpperC "g" = "G"
  ToUpperC "h" = "H"
  ToUpperC "i" = "I"
  ToUpperC "j" = "J"
  ToUpperC "k" = "K"
  ToUpperC "l" = "L"
  ToUpperC "m" = "M"
  ToUpperC "n" = "N"
  ToUpperC "o" = "O"
  ToUpperC "p" = "P"
  ToUpperC "q" = "Q"
  ToUpperC "r" = "R"
  ToUpperC "s" = "S"
  ToUpperC "t" = "T"
  ToUpperC "u" = "U"
  ToUpperC "v" = "V"
  ToUpperC "w" = "W"
  ToUpperC "x" = "X"
  ToUpperC "y" = "Y"
  ToUpperC "z" = "Z"
  ToUpperC a   = a
--------------------------------------------------------------------------------

-- | Convert the symbol to lowercase
type family ToLower (sym :: Symbol) :: Symbol where
  ToLower sym = ToLower1 (ToList sym)

type family ToLower1 (sym :: [Symbol]) :: Symbol where
  ToLower1 '[] = ""
  ToLower1 (x ': xs) = AppendSymbol (ToLowerC x) (ToLower1 xs)

type family ToLowerC (sym :: Symbol) :: Symbol where
  ToLowerC "A" = "a"
  ToLowerC "B" = "b"
  ToLowerC "C" = "c"
  ToLowerC "D" = "d"
  ToLowerC "E" = "e"
  ToLowerC "F" = "f"
  ToLowerC "G" = "g"
  ToLowerC "H" = "h"
  ToLowerC "I" = "i"
  ToLowerC "J" = "j"
  ToLowerC "K" = "k"
  ToLowerC "L" = "l"
  ToLowerC "M" = "m"
  ToLowerC "N" = "n"
  ToLowerC "O" = "o"
  ToLowerC "P" = "p"
  ToLowerC "Q" = "q"
  ToLowerC "R" = "r"
  ToLowerC "S" = "s"
  ToLowerC "T" = "t"
  ToLowerC "U" = "u"
  ToLowerC "V" = "v"
  ToLowerC "W" = "w"
  ToLowerC "X" = "x"
  ToLowerC "Y" = "y"
  ToLowerC "Z" = "z"
  ToLowerC a   = a

--------------------------------------------------------------------------------
-- | Parse a natural number
type family ReadNat (sym :: Symbol) :: Nat where
  ReadNat sym = ReadNat1 sym (ToList sym)

type family ReadNat1 (orig :: Symbol) (sym :: [Symbol]) :: Nat where
  ReadNat1 _ '[] = TypeError ('Text "Parse error: empty string")
  ReadNat1 orig xs  = ReadNat2 orig xs 0

type family ReadNat2 (orgin :: Symbol) (sym :: [Symbol]) (n :: Nat) :: Nat where
  ReadNat2 orig '[] acc = acc
  ReadNat2 orig (x ': xs) acc = ReadNat2 orig xs (10 * acc + ReadDigit orig x)

type family ReadDigit (orig :: Symbol) (sym :: Symbol) :: Nat where
  ReadDigit _ "0" = 0
  ReadDigit _ "1" = 1
  ReadDigit _ "2" = 2
  ReadDigit _ "3" = 3
  ReadDigit _ "4" = 4
  ReadDigit _ "5" = 5
  ReadDigit _ "6" = 6
  ReadDigit _ "7" = 7
  ReadDigit _ "8" = 8
  ReadDigit _ "9" = 9
  ReadDigit orig other =
    TypeError ('Text "Parse error: "
               ':<>: ShowType other
               ':<>: 'Text " is not a valid digit in "
               ':<>: ShowType orig)
