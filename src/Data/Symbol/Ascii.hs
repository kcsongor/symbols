{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Symbol.Ascii
  (
    Head
  ) where

import GHC.TypeLits

-- | Compute the first character of a type-level symbol
type family Head (sym :: Symbol) :: Symbol where
  Head "" = ""
  Head sym = Lookup sym Chars

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

type LookupTable = Tree (Symbol, Symbol)

type family Lookup (x :: Symbol) (xs :: LookupTable) :: Symbol where
  Lookup "" _ = ""
  Lookup x (Node l '(cl, cr) r) = Lookup2 (CmpSymbol cl x) (CmpSymbol cr x) x cl l r

type family Lookup2 ol or x cl l r :: Symbol where
  Lookup2 EQ _ _ cl _ _     = cl
  Lookup2 LT GT _ cl _ r   = cl
  Lookup2 LT _ _ cl _ Leaf = cl -- for the last character (~)
  Lookup2 LT _ x _ _ r      = Lookup x r
  Lookup2 GT _ x _ l _      = Lookup x l

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
