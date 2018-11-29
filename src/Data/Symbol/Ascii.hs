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

-- | Compute the first character of a type-level symbol
type family Head (sym :: Symbol) :: Symbol where
  Head "" = ""
  Head sym = Lookup sym "" Chars

-- | Convert the symbol into a list of characters
type family ToList (sym :: Symbol) :: [Symbol] where
  ToList sym = ToList1 sym Chars ""

--------------------------------------------------------------------------------
data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

type LookupTable = Tree (Symbol, Symbol)

type family ToList1 (sym :: Symbol) (table :: LookupTable) (prefix :: Symbol) :: [Symbol] where
  ToList1 sym table sym = '[]
  ToList1 sym table prefix = Lookup sym prefix table ': ToList1 sym table (AppendSymbol prefix (Lookup sym prefix table))

type family Lookup (x :: Symbol) (prefix :: Symbol) (xs :: LookupTable) :: Symbol where
  Lookup "" _ _ = ""
  Lookup x "" (Node l '(cl, cr) r) = Lookup2 (CmpSymbol cl x) (CmpSymbol cr x) x "" cl l r
  Lookup x prefix (Node l '(cl, cr) r) = Lookup2 (CmpSymbol (AppendSymbol prefix cl) x) (CmpSymbol (AppendSymbol prefix cr) x) x prefix cl l r

type family Lookup2 ol or x prefix cl l r :: Symbol where
  Lookup2 EQ _ _ _ cl _ _     = cl
  Lookup2 LT GT _ _ cl _ r    = cl
  Lookup2 LT _ _ _ cl _ Leaf  = cl -- for the last character (~)
  Lookup2 LT _ x prefix _ _ r = Lookup x prefix r
  Lookup2 GT _ x prefix _ l _ = Lookup x prefix l

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
  ReadNat sym = ReadNat1 (ToList sym) 0

type family ReadNat1 (sym :: [Symbol]) (n :: Nat) :: Nat where
  ReadNat1 '[] acc = acc
  ReadNat1 (x ': xs) acc = ReadNat1 xs (10 * acc + ReadDigit x)

type family ReadDigit (sym :: Symbol) :: Nat where
  ReadDigit "0" = 0
  ReadDigit "1" = 1
  ReadDigit "2" = 2
  ReadDigit "3" = 3
  ReadDigit "4" = 4
  ReadDigit "5" = 5
  ReadDigit "6" = 6
  ReadDigit "7" = 7
  ReadDigit "8" = 8
  ReadDigit "9" = 9

--------------------------------------------------------------------------------

-- | The search tree: each node contains two consecutive characters of
--   the printable ASCII charset, and we're looking for the node where
--   the first element is LT and the second element is GT than our
--   symbol
type Chars
 = 'Node
     ('Node
     ('Node
         ('Node
             ('Node
                 ('Node ('Node 'Leaf '(" ", "!") 'Leaf) '("!", "\"") 'Leaf)
                 '("\"", "#")
                 ('Node ('Node 'Leaf '("#", "$") 'Leaf) '("$", "%") 'Leaf))
             '("%", "&")
             ('Node
                 ('Node ('Node 'Leaf '("&", "'") 'Leaf) '("'", "(") 'Leaf)
                 '("(", ")")
                 ('Node ('Node 'Leaf '(")", "*") 'Leaf) '("*", "+") 'Leaf)))
         '("+", ",")
         ('Node
             ('Node
                 ('Node ('Node 'Leaf '(",", "-") 'Leaf) '("-", ".") 'Leaf)
                 '(".", "/")
                 ('Node ('Node 'Leaf '("/", "0") 'Leaf) '("0", "1") 'Leaf))
             '("1", "2")
             ('Node
                 ('Node ('Node 'Leaf '("2", "3") 'Leaf) '("3", "4") 'Leaf)
                 '("4", "5")
                 ('Node ('Node 'Leaf '("5", "6") 'Leaf) '("6", "7") 'Leaf))))
     '("7", "8")
     ('Node
         ('Node
             ('Node
                 ('Node ('Node 'Leaf '("8", "9") 'Leaf) '("9", ":") 'Leaf)
                 '(":", ";")
                 ('Node ('Node 'Leaf '(";", "<") 'Leaf) '("<", "=") 'Leaf))
             '("=", ">")
             ('Node
                 ('Node ('Node 'Leaf '(">", "?") 'Leaf) '("?", "@") 'Leaf)
                 '("@", "A")
                 ('Node ('Node 'Leaf '("A", "B") 'Leaf) '("B", "C") 'Leaf)))
         '("C", "D")
         ('Node
             ('Node
                 ('Node ('Node 'Leaf '("D", "E") 'Leaf) '("E", "F") 'Leaf)
                 '("F", "G")
                 ('Node ('Node 'Leaf '("G", "H") 'Leaf) '("H", "I") 'Leaf))
             '("I", "J")
             ('Node
                 ('Node ('Node 'Leaf '("J", "K") 'Leaf) '("K", "L") 'Leaf)
                 '("L", "M")
                 ('Node ('Node 'Leaf '("M", "N") 'Leaf) '("N", "O") 'Leaf)))))
     '("O", "P")
     ('Node
     ('Node
         ('Node
             ('Node
                 ('Node ('Node 'Leaf '("P", "Q") 'Leaf) '("Q", "R") 'Leaf)
                 '("R", "S")
                 ('Node ('Node 'Leaf '("S", "T") 'Leaf) '("T", "U") 'Leaf))
             '("U", "V")
             ('Node
                 ('Node ('Node 'Leaf '("V", "W") 'Leaf) '("W", "X") 'Leaf)
                 '("X", "Y")
                 ('Node ('Node 'Leaf '("Y", "Z") 'Leaf) '("Z", "[") 'Leaf)))
         '("[", "\\")
         ('Node
             ('Node
                 ('Node ('Node 'Leaf '("\\", "]") 'Leaf) '("]", "^") 'Leaf)
                 '("^", "_")
                 ('Node ('Node 'Leaf '("_", "`") 'Leaf) '("`", "a") 'Leaf))
             '("a", "b")
             ('Node
                 ('Node ('Node 'Leaf '("b", "c") 'Leaf) '("c", "d") 'Leaf)
                 '("d", "e")
                 ('Node ('Node 'Leaf '("e", "f") 'Leaf) '("f", "g") 'Leaf))))
     '("g", "h")
     ('Node
         ('Node
             ('Node
                 ('Node ('Node 'Leaf '("h", "i") 'Leaf) '("i", "j") 'Leaf)
                 '("j", "k")
                 ('Node ('Node 'Leaf '("k", "l") 'Leaf) '("l", "m") 'Leaf))
             '("m", "n")
             ('Node
                 ('Node ('Node 'Leaf '("n", "o") 'Leaf) '("o", "p") 'Leaf)
                 '("p", "q")
                 ('Node ('Node 'Leaf '("q", "r") 'Leaf) '("r", "s") 'Leaf)))
         '("s", "t")
         ('Node
             ('Node
                 ('Node ('Node 'Leaf '("t", "u") 'Leaf) '("u", "v") 'Leaf)
                 '("v", "w")
                 ('Node ('Node 'Leaf '("w", "x") 'Leaf) '("x", "y") 'Leaf))
             '("y", "z")
             ('Node
                 ('Node ('Node 'Leaf '("z", "{") 'Leaf) '("{", "|") 'Leaf)
                 '("|", "}")
                 ('Node ('Node 'Leaf '("}", "~") 'Leaf) '("~", "~") 'Leaf)))))
