module Lang where
  
import Data.Map (Map,fromList,lookup,insert)
import Data.Maybe (fromJust)
import Data.Char
import Prelude hiding (lookup)


--
-- * Abstract syntax
--

-- | Variables.
type Var = String
type FName = String

-- | Abstract syntax of expressions.
--
--     expr  ::=  int
--            |   expr + expr
--            |   expr ≤ expr
--            |   expr ≥ expr
--            |   `not` expr
--            |   var
--            
--
data Expr = Lit Int        -- literal integer
          | Add Expr Expr  -- integer addition
          | Mul Expr Expr  -- integer multiplication
          | Sub Expr Expr  -- integer subtraction
          | Mod Expr Expr  -- integer modulo
          | LTE Expr Expr  -- less than or equal to
          | GTE Expr Expr  -- greater than or equal to
          | EQL Expr Expr  -- equal to
          | LWR Expr Expr  -- less than
          | GTR Expr Expr  -- greater than
          | Not Expr       -- boolean negation
          | Ref Var        -- variable reference
          | Call FName [Var] -- function call
  deriving (Eq,Show)

-- | Abstract syntax of statements.
--
--     stmt  ::=  var := expr
--            |   `if` expr stmt `else` stmt
--            |   `while` expr stmt
--            |   { stmt* }
--
data Stmt = Bind Var Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Block [Stmt]
  deriving (Eq,Show)

-- | Abstract syntax of functions.
data Func = F FName [Decl] Stmt
  deriving (Eq,Show)

-- | Abstract syntax of strings
--
--    str ::= string
--          | string ++ string
--          | toUpper string

data Str = MyStr String
          | Concat Str Str
          | Upper Str
  deriving (Eq,Show)

-- | Abstract syntax of types.
--     
--     type  ::=  `int`  |  `bool`
--
data Type = TInt | TBool
  deriving (Eq,Show)

-- | Abstract syntax of declarations.
--
--     decl  ::=  var : type
--
type Decl = (Var,Type)

-- | Abstract syntax of programs.
--
--     prog  ::=  decl* `begin` stmt
--
data Prog = P [Func] [Decl] Stmt
  deriving (Eq,Show)

-- Good examples for custom Str string data type
myStr1 :: Str
myStr1 = MyStr "hi world"

myStr2 :: Str
myStr2 = Concat (MyStr "hi ") (MyStr "world")

myStr3 :: Str
myStr3 = Upper (MyStr "hello")

-- | Example good program: sum all of the numbers from 1 to 100.
--
--     sum : int
--     n : int
--     begin {
--       sum := 0
--       n := 1
--       while n <= 100 {
--         sum := sum + n
--         n := n + 1
--       }
--     }
ex1 :: Prog
ex1 = P 
      [] 

      [("sum",TInt),("n",TInt)]
      (Block [
        Bind "sum" (Lit 0),
        Bind "n" (Lit 1),
        While (LTE (Ref "n") (Lit 100))
        (Block [
          Bind "sum" (Add (Ref "sum") (Ref "n")),
          Bind "n" (Add (Ref "n") (Lit 1))
        ])
      ])

-- | Example good program: tests the use of functions by defining the "square" function that squares an int
ex2 :: Prog
ex2 = P 
      [F "square" [("return", TInt)] 
      (Block [
        Bind "return" (Mul (Ref "return") (Ref "return"))
      ])] 

      [("n",TInt)]
      (Block [
        Bind "n" (Lit 4),
        Bind "n" (Call "square" ["n"])
      ])

-- | Example bad program: type error.
--
--     x : int
--     begin
--       x := 3 <= 4
--
ex3 :: Prog
ex3 = P 
      [] 
      
      [("x",TInt)] (Bind "x" (LTE (Lit 3) (Lit 4)))

-- | Example program: Using strings.
--
-- ex4 :: Prog
-- ex4 = P 
--       [] 
      
--       [("x",Str)] (Bind "testStr" (Str "this is a test string"))
      
-- | Example good program: tests recursion on euclid's algorithm for greatest common denominator
ex5 :: Prog
ex5 = P 
      [F "gcd" [("return", TInt), ("b", TInt)] 
      (Block [
        If (EQL (Ref "return") (Lit 0)) 
        -- then
        (Block[
          Bind "return" (Ref "b")
        ])
        -- else
        (Block[
          Bind "b" (Mod (Ref "b") (Ref "a")),
          Bind "return" (Call "gcd" ["b", "return"])
        ])
      ])] 

      [("a",TInt), ("b", TInt), ("res1", TInt), ("res2", TInt)]
      (Block [
        Bind "a" (Lit 4),
        Bind "b" (Lit 12),
        Bind "res1" (Call "gcd" ["a", "b"]),
        Bind "a" (Lit 12),
        Bind "b" (Lit 36),
        Bind "res2" (Call "gcd" ["a", "b"])
      ])


--
-- * Type system
--

-- | Variable environments. An environment maps variable names to the
--   things those variable names are bound to. During typing, each name
--   will be bound to a type, while during evaluation (semantics), each
--   name will be bound to a value.
type Env a = Map Var a

-- | Typing relation for expressions. We need an environment to lookup
--   the names of variable references (last case). We use a Maybe to
--   represent the fact that typing might fail, for example, if we get
--   a type error or if a variable is not in the environment.
typeExpr :: Expr -> [Func] -> Env Type -> Maybe Type
typeExpr (Lit _)      _ _ = Just TInt
typeExpr (Add l r)    f m = case (typeExpr l f m, typeExpr r f m) of
                            (Just TInt, Just TInt) -> Just TInt
                            _                      -> Nothing
typeExpr (Sub l r)    f m = case (typeExpr l f m, typeExpr r f m) of
                            (Just TInt, Just TInt) -> Just TInt
                            _                      -> Nothing
typeExpr (Mod l r)    f m = case (typeExpr l f m, typeExpr r f m) of
                            (Just TInt, Just TInt) -> Just TInt
                            _                      -> Nothing
typeExpr (Mul l r)    f m = case (typeExpr l f m, typeExpr r f m) of
                            (Just TInt, Just TInt) -> Just TInt
                            _                      -> Nothing
typeExpr (LTE l r)    f m = case (typeExpr l f m, typeExpr r f m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing
typeExpr (GTE l r)    f m = case (typeExpr l f m, typeExpr r f m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing
typeExpr (EQL l r)    f m = case (typeExpr l f m, typeExpr r f m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing
typeExpr (LWR l r)    f m = case (typeExpr l f m, typeExpr r f m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing
typeExpr (GTR l r)    f m = case (typeExpr l f m, typeExpr r f m) of
                            (Just TInt, Just TInt) -> Just TBool
                            _                      -> Nothing
typeExpr (Not e)      f m = case typeExpr e f m of
                            Just TBool             -> Just TBool
                            _                      -> Nothing
typeExpr (Ref v)      f m = lookup v m
typeExpr (Call r vs)  f m = case findFunc r f of
                            Just (F _ ds s) -> case typeStmt s f mf of
                              True -> lookup "return" (fromList ds)
                              False -> Nothing
                              where mf = case listConv (map (\t -> lookup t m) vs) of
                                          Just tl  -> fromList (zipWith (\(a,b) c -> (a, c)) ds tl)
                                          Nothing  -> error "internal error: expected function parameters do not match entered parameters"
                            _                      -> Nothing
-- data Func = F FName [Decl] Stmt
--   deriving (Eq,Show)
-- type Decl = (Var,Type)
-- type Env a = Map Var a

-- | Type checking statements. Note that the return type here is just a
--   Boolean value since a statement doesn't have a type. The Boolean
--   value indicates whether or not the statement is type correct (i.e.
--   this function implements type checking of statements).
-- 
--   Also note that when we type check a while loop, we do not execute
--   the loop several times; we just check that the type of the condition
--   is a Bool, and check that the body type checks. If both of those are
--   true, then we know that the while loop cannot produce a type error
--   without having to consider each iteration. This is where the benefits
--   of static type checking start to become more clear. With dynamic
--   typing we would have to check the types in the loop body on each
--   iteration, whereas with static typing we just check the loop body
--   once.
typeStmt :: Stmt -> [Func] -> Env Type -> Bool
typeStmt (Bind v e)   f m = case (lookup v m, typeExpr e f m) of
                            (Just tv, Just te) -> tv == te
                            _ -> False
typeStmt (If c st se) f m = case typeExpr c f m of
                            Just TBool -> typeStmt st f m && typeStmt se f m
                            _ -> False
typeStmt (While c sb) f m = case typeExpr c f m of
                            Just TBool -> typeStmt sb f m
                            _ -> False
typeStmt (Block ss)   f m = all (\s -> typeStmt s f m) ss


-- | Type checking programs. The 'fromList' function is from the
--   Data.Map module. It builds a map from a list of pairs, thus
--   initializing our typing environment.
typeProg :: Prog -> Bool
typeProg (P fs ds s) = typeStmt s fs (fromList ds)


--
-- * Semantics
--

-- | The basic values in our language.
type Val = Either Int Bool

-- | Semantics of type-correct expressions. Note that since we assume the
--   expression is statically type correct (otherwise it would have failed
--   type checking and we never try to evaluate it), we do not need to
--   explicitly represent the error case with a Maybe type. Also note that
--   our environment now contains the *value* that each name is bound to.
--   Since expressions can refer to variables but not change them, the
--   environment is read-only (i.e. it's an input but not an output of the
--   function).
evalExpr :: Expr -> [Func] -> Env Val -> Val
evalExpr (Lit i)   _ _ = Left i
evalExpr (Add l r) f m = Left (evalInt l f m + evalInt r f m)
evalExpr (Sub l r) f m = Left (evalInt l f m + evalInt r f m)
evalExpr (Mod l r) f m = Left (mod (evalInt l f m) (evalInt r f m))
evalExpr (Mul l r) f m = Left (evalInt l f m * evalInt r f m)
evalExpr (LTE l r) f m = Right (evalInt l f m <= evalInt r f m)
evalExpr (GTE l r) f m = Right (evalInt l f m >= evalInt r f m)
evalExpr (EQL l r) f m = Right (evalInt l f m == evalInt r f m)
evalExpr (LWR l r) f m = Right (evalInt l f m < evalInt r f m)
evalExpr (GTR l r) f m = Right (evalInt l f m < evalInt r f m)
evalExpr (Not e)   f m = Right (not (evalBool e f m))
evalExpr (Ref x)   f m = case lookup x m of
                         Just v  -> v
                         Nothing -> error "internal error: undefined variable"
evalExpr (Call r vs)  f m = evalFunc r vs f m


findFunc :: FName -> [Func] -> Maybe Func
findFunc n [] = Nothing
findFunc n ((F n' ds s):nx) = if n == n'
                              then Just (F n' ds s)
                              else findFunc n nx

listConv :: [Maybe a] -> Maybe [a]
listConv [] = Just []
listConv (Nothing:n) = Nothing
listConv ((Just c):n) = case listConv n of
                          Just vs -> Just (c:vs)
                          Nothing -> Nothing

evalFunc :: FName -> [Var] -> [Func] -> Env Val -> Val
evalFunc fn vs f m = case findFunc fn f of
                      Nothing -> error "internal error: function not defined"
                      Just (F _ ds s) -> case lookup "return" (evalStmt s f mf) of
                        Just r  -> r
                        Nothing -> error "internal error: function 'return' var not defined"
                        where mf = case listConv (map (\v -> lookup v m) vs) of
                                    Just vl  -> fromList (zipWith (\(a,b) c -> (a, c)) ds vl)
                                    Nothing  -> error "internal error: on function call, mismatch of referenced variables and function requirements" 

-- | Helper function to evaluate an expression to an integer. Note that
--   in all cases, we should only get an "internal error" if we try to
--   evaluate an expression that didn't pass the static type checker we
--   wrote above.
evalInt :: Expr -> [Func] -> Env Val -> Int
evalInt e f m = case evalExpr e f m of
                Left i  -> i
                Right _ -> error "internal error: expected Int got Bool"

-- | Helper function to evaluate an expression to a Boolean.
evalBool :: Expr -> [Func] -> Env Val -> Bool
evalBool e f m = case evalExpr e f m of
                 Right b -> b
                 Left _  -> error "internal error: expected Bool got Int"

-- | Semantics of statements. Statements update the bindings in the
--   environment, so the semantic domain is 'Env Val -> Env Val'. The
--   bind case is the case that actually changes the environment. The
--   other cases should look similar to other examples you've seen.
evalStmt :: Stmt -> [Func] -> Env Val -> Env Val
evalStmt (Bind x e)   f m = insert x (evalExpr e f m) m
evalStmt (If c st se) f m = if evalBool c f m
                          then evalStmt st f m
                          else evalStmt se f m
evalStmt (While c sb) f m = if evalBool c f m
                          then evalStmt (While c sb) f (evalStmt sb f m)
                          else m
evalStmt (Block ss)   f m = evalStmts ss f m

-- | Helper function to evaluate a list of statements. We could also
--   have used 'foldl' here.
evalStmts :: [Stmt] -> [Func] -> Env Val -> Env Val
evalStmts []     f m = m
evalStmts (s:ss) f m = evalStmts ss f (evalStmt s f m)

-- | Semantics of programs. This runs a program with an initial
--   environment where all integer variables are initialized to 0, and
--   all Boolean variables are initialized to false.
evalProg :: Prog -> Env Val
evalProg (P fs ds s) = evalStmt s fs m
  where
    m = fromList (map (\(x,t) -> (x, init t)) ds)
    init TInt  = Left 0
    init TBool = Right False

-- | Type check and then run a program.
runProg :: Prog -> Maybe (Env Val)
runProg p = if typeProg p then Just (evalProg p)
                          else Nothing

-- Semantics for using the Str data type
evalString :: Str -> String
evalString (MyStr s)      = s
evalString (Concat s1 s2) = (evalString s1) ++ (evalString s2)
evalString (Upper s)      = map toUpper (evalString s)