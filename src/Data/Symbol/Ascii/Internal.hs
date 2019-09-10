{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Symbol.Ascii.Internal where

import Prelude hiding (head, lookup)

import Data.Char (chr)
import GHC.TypeLits (CmpSymbol, Symbol, AppendSymbol, ErrorMessage (..), TypeError)

#ifdef MIN_VERSION_QuickCheck
import Test.QuickCheck
#endif

-- $setup
-- >>> :set -XDataKinds

-------------------------------------------------------------------------------
-- term-level
-------------------------------------------------------------------------------

type M = Either String

head :: String -> M String
head ""  = Right ""
head sym = head1 sym (compare sym "\128")

head1 :: String -> Ordering -> M String
head1 sym GT = Left $ "Starts with non-ASCII character " ++ sym
head1 sym _  = lookup sym "" chars

toList :: String -> M [String]
toList sym = toList1 sym ""

toList1 :: String -> String -> M [String]
toList1 x pfx
  | x == pfx  = Right []
  | otherwise = toList2 x pfx (compare x (pfx ++ "\128"))

toList2 :: String -> String -> Ordering -> M [String]
toList2 x pfx LT = do
  h <- lookup x pfx chars
  t <- toList1 x (pfx ++ h)
  return (h : t)
toList2 x pfx o  = Left $ "Non-ASCII " ++ show (x, pfx, o)

lookup :: String ->  String -> Tree String -> M String
lookup "" _   _            = Right ""
lookup _  _   (Leaf x)     = Right x
lookup x  ""  (Node l c r) = lookup2 x ""  c (compare x c)  l r
lookup x  pfx (Node l c r) = lookup2 x pfx c (compare x (pfx ++ c)) l r

lookup2 :: String -> String -> String -> Ordering -> Tree String -> Tree String -> M String
lookup2 x pfx c o l r = case o of
  EQ -> Right c
  LT -> lookup x pfx l
  GT -> lookup x pfx r

#ifdef MIN_VERSION_QuickCheck

-- | >>> quickCheck head_prop
-- +++ OK, passed 100 tests:
-- ...
head_prop :: ASCIIString -> Property
head_prop (ASCIIString [])         = label "empty" True
head_prop (ASCIIString xs@(x : _)) = label "non-empty" $ head xs === Right [x]

-- | >>> quickCheck headError_prop
-- +++ OK, passed 100 tests:
-- ...
headError_prop :: String -> Property
headError_prop [] = label "empty" True
headError_prop xs@(x : _)
  | x < chr 128 = label "ascii" $ head xs === Right [x]
  | otherwise   = label "non-ascii" $ case head xs of
    Left _    -> property True
    Right res -> counterexample (show res) False

-- | >>> quickCheck toList_prop
-- +++ OK, passed 100 tests.
toList_prop :: ASCIIString -> Property
toList_prop (ASCIIString s) = toList s === Right (map (:[]) s)

-- | >>> quickCheck toListError_prop
-- +++ OK, passed 100 tests; ...
toListError_prop :: ASCIIString -> Char -> ASCIIString -> Property
toListError_prop (ASCIIString xs) y (ASCIIString zs) =
    label (if ascii then "ascii" else "non-ascii") $
        not ascii ==> case toList (xs ++ [y] ++ zs) of
            Left _    -> property True
            Right res -> counterexample (show res) False
  where
    ascii = y < chr 128

#endif

-------------------------------------------------------------------------------
-- type-level
-------------------------------------------------------------------------------

-- | Compute the first character of a type-level symbol
--
-- >>> :kind! Head "Example"
-- Head "Example" :: Symbol
-- = "E"
--
-- >>> :kind! Head ""
-- Head "" :: Symbol
-- = ""
--
-- 'Head' doesn't fail if the first character is ASCII, rest is irrelevant
--
-- >>> :kind! Head "123±456"
-- Head "123±456" :: Symbol
-- = "1"
--
-- 'Head' fails if the first character is non-ASCII
--
-- >>> :kind! Head "±123"
-- Head "±123" :: Symbol
-- = (TypeError ...)
--
type family Head (sym :: Symbol) :: Symbol where
  Head ""  = ""
  Head sym = Head1 sym (CmpSymbol sym "\128")

-- | Convert the symbol into a list of characters
--
-- >>> :kind! ToList "ABC"
-- ToList "ABC" :: [Symbol]
-- = '["A", "B", "C"]
--
-- 'ToList' works only for ASCII strings
--
-- >>> :kind! ToList "123±456"
-- ToList "123±456" :: [Symbol]
-- = "1" : "2" : "3" : (TypeError ...)
--
type family ToList (sym :: Symbol) :: [Symbol] where
  ToList sym = ToList1 sym ""

-------------------------------------------------------------------------------

type family Head1 (x :: Symbol) (o :: Ordering) :: Symbol where
  Head1 x 'GT = TypeError ('Text "Starts with non-ASCII character " ':<>: ShowType x)
  Head1 x _   = Lookup x "" Chars

type family ToList1 (x :: Symbol) (pfx :: Symbol) :: [Symbol] where
  ToList1 x x   = '[]
  ToList1 x pfx = ToList2 x pfx (CmpSymbol x (AppendSymbol pfx "\128"))

type family ToList2 (x :: Symbol) (pfx :: Symbol) (o :: Ordering) :: [Symbol] where
  ToList2 x pfx 'LT = Lookup x pfx Chars ': ToList1 x (AppendSymbol pfx (Lookup x pfx Chars))
  ToList2 x _   _   = TypeError ('Text "Non-AScII character in " ':<>: ShowType x)

type family Lookup (x :: Symbol) (pfx :: Symbol) (xs :: Tree Symbol) :: Symbol where
  Lookup "" _   _             = ""
  Lookup _  _   ('Leaf x)     = x
  Lookup x  ""  ('Node l c r) = Lookup2 x ""  c (CmpSymbol x c)                    l r
  Lookup x  pfx ('Node l c r) = Lookup2 x pfx c (CmpSymbol x (AppendSymbol pfx c)) l r

type family Lookup2 (x :: Symbol) (pfx :: Symbol) (c :: Symbol) (o :: Ordering) (l :: Tree Symbol) (r :: Tree Symbol) :: Symbol where
  Lookup2 _ _   c 'EQ _ _ = c
  Lookup2 x pfx c 'LT l _ = Lookup x pfx l
  Lookup2 x pfx _ 'GT _ r = Lookup x pfx r

-------------------------------------------------------------------------------
-- Search Tree
-------------------------------------------------------------------------------

-- | The search tree. Each leaf contains final element.
data Tree a
  = Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show)

chars :: Tree String
chars = buildTree [ chr c | c <- [0..0x7f] ] where
  buildTree []    = error "panic! buildTree []"
  buildTree [c]   = Leaf [c]
  buildTree pairs = Node (buildTree l) c (buildTree r) where
    n      = length pairs
    (l, r) = splitAt (n `div` 2) pairs
    c      = case r of
      []     -> error "panic! buildTree: r is empty"
      (c':_) -> [c']

-- To print this tree using pretty-show
-- *Data.Symbol.Ascii.Internal Text.Show.Pretty Data.Maybe> valToDoc $ fromJust $ parseValue $ show chars
--
type Chars = Node
  (Node
     (Node
        (Node
           (Node
              (Node
                 (Node (Leaf "\NUL") "\SOH" (Leaf "\SOH"))
                 "\STX"
                 (Node (Leaf "\STX") "\ETX" (Leaf "\ETX")))
              "\EOT"
              (Node
                 (Node (Leaf "\EOT") "\ENQ" (Leaf "\ENQ"))
                 "\ACK"
                 (Node (Leaf "\ACK") "\a" (Leaf "\a"))))
           "\b"
           (Node
              (Node
                 (Node (Leaf "\b") "\t" (Leaf "\t"))
                 "\n"
                 (Node (Leaf "\n") "\v" (Leaf "\v")))
              "\f"
              (Node
                 (Node (Leaf "\f") "\r" (Leaf "\r"))
                 "\SO"
                 (Node (Leaf "\SO") "\SI" (Leaf "\SI")))))
        "\DLE"
        (Node
           (Node
              (Node
                 (Node (Leaf "\DLE") "\DC1" (Leaf "\DC1"))
                 "\DC2"
                 (Node (Leaf "\DC2") "\DC3" (Leaf "\DC3")))
              "\DC4"
              (Node
                 (Node (Leaf "\DC4") "\NAK" (Leaf "\NAK"))
                 "\SYN"
                 (Node (Leaf "\SYN") "\ETB" (Leaf "\ETB"))))
           "\CAN"
           (Node
              (Node
                 (Node (Leaf "\CAN") "\EM" (Leaf "\EM"))
                 "\SUB"
                 (Node (Leaf "\SUB") "\ESC" (Leaf "\ESC")))
              "\FS"
              (Node
                 (Node (Leaf "\FS") "\GS" (Leaf "\GS"))
                 "\RS"
                 (Node (Leaf "\RS") "\US" (Leaf "\US"))))))
     " "
     (Node
        (Node
           (Node
              (Node
                 (Node (Leaf " ") "!" (Leaf "!"))
                 "\""
                 (Node (Leaf "\"") "#" (Leaf "#")))
              "$"
              (Node
                 (Node (Leaf "$") "%" (Leaf "%"))
                 "&"
                 (Node (Leaf "&") "'" (Leaf "'"))))
           "("
           (Node
              (Node
                 (Node (Leaf "(") ")" (Leaf ")"))
                 "*"
                 (Node (Leaf "*") "+" (Leaf "+")))
              ","
              (Node
                 (Node (Leaf ",") "-" (Leaf "-"))
                 "."
                 (Node (Leaf ".") "/" (Leaf "/")))))
        "0"
        (Node
           (Node
              (Node
                 (Node (Leaf "0") "1" (Leaf "1"))
                 "2"
                 (Node (Leaf "2") "3" (Leaf "3")))
              "4"
              (Node
                 (Node (Leaf "4") "5" (Leaf "5"))
                 "6"
                 (Node (Leaf "6") "7" (Leaf "7"))))
           "8"
           (Node
              (Node
                 (Node (Leaf "8") "9" (Leaf "9"))
                 ":"
                 (Node (Leaf ":") ";" (Leaf ";")))
              "<"
              (Node
                 (Node (Leaf "<") "=" (Leaf "="))
                 ">"
                 (Node (Leaf ">") "?" (Leaf "?")))))))
  "@"
  (Node
     (Node
        (Node
           (Node
              (Node
                 (Node (Leaf "@") "A" (Leaf "A"))
                 "B"
                 (Node (Leaf "B") "C" (Leaf "C")))
              "D"
              (Node
                 (Node (Leaf "D") "E" (Leaf "E"))
                 "F"
                 (Node (Leaf "F") "G" (Leaf "G"))))
           "H"
           (Node
              (Node
                 (Node (Leaf "H") "I" (Leaf "I"))
                 "J"
                 (Node (Leaf "J") "K" (Leaf "K")))
              "L"
              (Node
                 (Node (Leaf "L") "M" (Leaf "M"))
                 "N"
                 (Node (Leaf "N") "O" (Leaf "O")))))
        "P"
        (Node
           (Node
              (Node
                 (Node (Leaf "P") "Q" (Leaf "Q"))
                 "R"
                 (Node (Leaf "R") "S" (Leaf "S")))
              "T"
              (Node
                 (Node (Leaf "T") "U" (Leaf "U"))
                 "V"
                 (Node (Leaf "V") "W" (Leaf "W"))))
           "X"
           (Node
              (Node
                 (Node (Leaf "X") "Y" (Leaf "Y"))
                 "Z"
                 (Node (Leaf "Z") "[" (Leaf "[")))
              "\\"
              (Node
                 (Node (Leaf "\\") "]" (Leaf "]"))
                 "^"
                 (Node (Leaf "^") "_" (Leaf "_"))))))
     "`"
     (Node
        (Node
           (Node
              (Node
                 (Node (Leaf "`") "a" (Leaf "a"))
                 "b"
                 (Node (Leaf "b") "c" (Leaf "c")))
              "d"
              (Node
                 (Node (Leaf "d") "e" (Leaf "e"))
                 "f"
                 (Node (Leaf "f") "g" (Leaf "g"))))
           "h"
           (Node
              (Node
                 (Node (Leaf "h") "i" (Leaf "i"))
                 "j"
                 (Node (Leaf "j") "k" (Leaf "k")))
              "l"
              (Node
                 (Node (Leaf "l") "m" (Leaf "m"))
                 "n"
                 (Node (Leaf "n") "o" (Leaf "o")))))
        "p"
        (Node
           (Node
              (Node
                 (Node (Leaf "p") "q" (Leaf "q"))
                 "r"
                 (Node (Leaf "r") "s" (Leaf "s")))
              "t"
              (Node
                 (Node (Leaf "t") "u" (Leaf "u"))
                 "v"
                 (Node (Leaf "v") "w" (Leaf "w"))))
           "x"
           (Node
              (Node
                 (Node (Leaf "x") "y" (Leaf "y"))
                 "z"
                 (Node (Leaf "z") "{" (Leaf "{")))
              "|"
              (Node
                 (Node (Leaf "|") "}" (Leaf "}"))
                 "~"
                 (Node (Leaf "~") "\DEL" (Leaf "\DEL")))))))
