-- Homework 3 for the Oregon State University programming language fundamentals class
-- Created by: Isaac Peters
module HW3 where

import Prelude hiding (Num)

-- Task 1, define the abstract syntax of MiniLogo
type Num    = Int
type Var    = String 
type Macro  = String

type Prog = [Cmd]

data Mode = Down | Up
  deriving (Eq,Show)

data Expr = Ref Var
          | Lit Num
          | Add Expr Expr
  deriving (Eq,Show)

data Cmd  = Pen Mode
          | Move Expr Expr
          | Define Macro [Var] Prog
          | Call Macro [Expr]
  deriving (Eq,Show)

-- Task 2, define the line macro for MiniLogo
    -- define line(x1, y1, x2, y2) {
    --   pen up; move (x1, y1);
    --   pen down; move (x2, y2);
    -- }

line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"] [
  Pen Up, Move (Ref "x1") (Ref "y1"),
  Pen Down, Move (Ref "x2") (Ref "y2")]

-- Task 3, define the nix macro for MiniLogo
    -- define nix (x, y, w, h) {
    --   call line(x, y, x+w, y+h);
    --   call line(x, y+h, x+w, y);
    -- }

nix :: Cmd
nix = Define "Cmd" ["x", "y", "w", "h"] [
  Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")],
  Call "line" [Ref "x", Add (Ref "y") (Ref "h"), Add (Ref "x") (Ref "w"), Ref "y"]]

-- Task 4, define a haskell function that generates a steps macro for MiniLogo
steps :: Int -> Prog
steps 0 = [Pen Up, Move (Lit 0) (Lit 0), Pen Down]
steps n = steps (n-1) ++ [Move (Lit (n-1)) (Lit n), Move (Lit (n-1)) (Lit (n-1))]

-- Task 5, define a haskell function that returns all macros defined in a function
macros :: Prog -> [Macro]
macros [] = []
macros ((Define m _ _):n) = [m] ++ macros n
macros (_:n) = macros n

-- Task 6, define a haskell function taht pretty-prints a MiniLogo program
pretty :: Prog -> String
pretty [] = ""
pretty (c:n) = prettyCmd c ++ pretty n

prettyCmd :: Cmd -> String
prettyCmd (Pen Down) = "\tPen Down;\n"
prettyCmd (Pen Up) = "\tPen Down;\n"
prettyCmd (Move x1 x2) = "\tmove (" ++ (prettyExpr x1) 
                      ++ ", " ++ (prettyExpr x2) ++ ");\n" 
prettyCmd (Define m vs p) = "define " ++ m ++ " (" ++ prettyVars vs ++ ") {\n"
                      ++ pretty p
                      ++ "}\n"
prettyCmd (Call m exN) = "\tcall " ++ m ++ " (" ++ prettyExprs exN ++ ");\n"

prettyExpr :: Expr -> String
prettyExpr (Ref v) = v
prettyExpr (Lit x) = show x
prettyExpr (Add x1 x2) = prettyExpr x1 ++ " + " ++ prettyExpr x2
-- pretty ((Ref v):n) = v ++ ", " ++ pretty n

prettyVars :: [Var] -> String
prettyVars [c] = c
prettyVars (c:n) = c ++ ", " ++ prettyVars n

prettyExprs :: [Expr] -> String
prettyExprs [c] = prettyExpr c
prettyExprs (c:n) = prettyExpr c ++ ", " ++ prettyExprs n

