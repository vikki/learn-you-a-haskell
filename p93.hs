module P93 where
 
import Control.Monad
import Data.List
import Data.Maybe
 
type Equation = (Expr, Expr)
data Expr = Const Integer | Binary Expr Op Expr
        deriving (Eq, Show)
data Op = Plus | Minus | Multiply | Divide
        deriving (Bounded, Eq, Enum, Show)
type Value = Rational
 
-- top-level function: all correct equations generated from the list of
-- numbers, as pretty strings.
puzzle :: [Integer] -> [String]
puzzle ns = map (flip showsEquation "") (equations ns)
 
-- generate all correct equations from the list of numbers
equations :: [Integer] -> [Expr]
--equations :: [Integer] -> [([Integer],[Integer])]
equations [] = error "empty list of numbers"
equations [n] = error "only one number"
--equations ns = [(ns1, ns2) |
                --(ns1, ns2) <- splits ns]
                
equations ns = [e1 |
                (e1, v1) <- (exprs.init) ns,
                --v1 == last ns]
                v1 == 9]
 
-- generate all expressions from the numbers, except those containing
-- a division by zero, or redundant right-associativity.
-- so I think that op is figuring out that it is of Op type
-- based of of the return type being Expr and its inferring that op has to be type then
-- so it can do minBound..maxBound because its a Bounded type! phew!
-- exprs recursively builds up the value of each expression, so it does each bit of the expression and combines the values recursively
-- i.e. given 4+5-6 it would do 4+5, then combine that with -6 to get the result
-- and it does this on each tuple that its given
exprs :: [Integer] -> [(Expr, Value)]
exprs [n] = [(Const n, fromInteger n)]
exprs ns = [(Binary e1 op e2, v) | (ns1, ns2) <- splits ns,
                (e1, v1) <- exprs ns1,
                (e2, v2) <- exprs ns2,
                op <- [minBound..maxBound],
                not (right_associative op e2),
                v <- maybeToList (apply op v1 v2)]
 
-- splittings of a list into two non-empty lists
splits :: [a] -> [([a],[a])]
splits xs = tail (init (zip (inits xs) (tails xs)))
 
-- applying an operator to arguments may fail (division by zero)
apply :: Op -> Value -> Value -> Maybe Value
apply Plus x y = Just (x + y)
apply Minus x y = Just (x - y)
apply Multiply x y = Just (x * y)
apply Divide x 0 = Nothing
apply Divide x y = Just (x / y)
 
-- e1 op (e2 op' e3) == (e1 op e2) op' e3
-- check if have any unnecessary brackets in our equations!
-- so there's no need to wrap 2+4 in brackets if we're just adding stuff to it
-- because it'll make no difference to the result
-- test out with calls like right_associative Plus (Binary (Const 2) Plus (Const 4))
right_associative :: Op -> Expr -> Bool
right_associative Plus (Binary _ Plus _) = True
right_associative Plus (Binary _ Minus _) = True
right_associative Multiply (Binary _ Multiply _) = True
right_associative Multiply (Binary _ Divide _) = True
right_associative _ _ = False
 
-- Printing of equations and expressions
 
showsEquation :: Expr -> ShowS
showsEquation l = showsExprPrec 0 l 
 
-- all operations are left associative
showsExprPrec :: Int -> Expr -> ShowS
showsExprPrec _ (Const n) = shows n
showsExprPrec p (Binary e1 op e2) = showParen (p > op_prec) $
        showsExprPrec op_prec e1 . showString (opName op) .
                showsExprPrec (op_prec+1) e2
  where op_prec = precedence op
 
precedence :: Op -> Int
precedence Plus = 6
precedence Minus = 6
precedence Multiply = 7
precedence Divide = 7
 
opName :: Op -> String
opName Plus = "+"
opName Minus = "-"
opName Multiply = "*"
opName Divide = "/"

-- this will give you all of the possible values of Op, proving my point about exprs ( see comment)
wibble :: [Int] -> [Op]
wibble x = [op | op <- [minBound..maxBound]]

badger :: [Int] -> [Int]
badger xs = [x*2 | x <- xs]
