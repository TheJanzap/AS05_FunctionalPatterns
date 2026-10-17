{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module Lang.Imp where

-- Here we use an existing Map from the containers package.
import qualified Data.Map.Strict as M
import Control.Monad (foldM)

todo :: a
todo = error "TODO"

type Name = String
type State = M.Map Name Integer

emptyState :: State
emptyState = M.empty

data Expr a where
    BNot :: Expr Bool -> Expr Bool
    BAnd :: Expr Bool -> Expr Bool -> Expr Bool
    BOr  :: Expr Bool -> Expr Bool -> Expr Bool

    REq :: Eq a => Expr a -> Expr a -> Expr Bool
    RLt :: Expr Integer -> Expr Integer -> Expr Bool

    AConst :: Integer -> Expr Integer
    APlus  :: Expr Integer -> Expr Integer -> Expr Integer
    AMinus :: Expr Integer -> Expr Integer -> Expr Integer
    AMul   :: Expr Integer -> Expr Integer -> Expr Integer
    ADiv   :: Expr Integer -> Expr Integer -> Expr Integer
    AMod   :: Expr Integer -> Expr Integer -> Expr Integer
    AVar   :: Name -> Expr Integer

data Cmd = 
    CSkip
  | CAssign Name (Expr Integer)
  | CIfThEl (Expr Bool) Cmd Cmd
  | CWhile (Expr Bool) Cmd
  | CSeq [Cmd]


-- ImpExpr: Value which takes a state and either returns a value (Right) or an error (Left).
newtype ImpExpr a = ImpExpr { runExpr :: State -> Either String a }


-- The Functor instance for ImpExpr:
instance Functor ImpExpr where
  fmap :: (a -> b) -> ImpExpr a -> ImpExpr b
  fmap f ma = ma >>= pure . f


-- The Applicative instance for ImpExpr:
instance Applicative ImpExpr where
  pure :: a -> ImpExpr a
  pure a = ImpExpr (\_ -> Right a) 

  (<*>) :: ImpExpr (a -> b) -> ImpExpr a -> ImpExpr b
  mf <*> ma = do
   f <- mf
   a <- ma
   pure (f a)


-- The Monad instance for ImpExpr:
instance Monad ImpExpr where
  (>>=) :: ImpExpr a -> (a -> ImpExpr b) -> ImpExpr b
  ImpExpr a >>= f = ImpExpr (\st -> 
    a st >>= \x -> runExpr (f x) st) -- the (>>=) on this line is the one from Either


-- Reads a variable from the state. Fails with Left if the variable is not found.
-- Built on top of more fundamental functions (better versions).
readVariable :: Name -> ImpExpr Integer
readVariable n = do
  st <- ask
  maybe (failWith $ "variable not found: " ++ n) pure (M.lookup n st)


-- Returns the current state.
ask :: ImpExpr State
ask = ImpExpr $ \st -> Right st


-- Fails with Left and the given error message.
failWith :: String -> ImpExpr a
failWith s = ImpExpr $ \_ -> Left s


-- TODO: Implement the eval function.
-- Make sure to handle division by zero and variable not found errors.
eval :: Expr a -> ImpExpr a
eval (BNot b)     = fmap not (eval b)
eval (BAnd a b)   = pure (&&) <*> eval a <*> eval b
eval (BOr a b)    = pure (||) <*> eval a <*> eval b

eval (REq a b)    = pure (==) <*> eval a <*> eval b
eval (RLt a b)    = pure (<) <*> eval a <*> eval b

eval (AVar n)     = readVariable n
eval (AConst n)   = pure n -- unpack the constant and return it
eval (APlus a b)  = pure (+) <*> eval a <*> eval b
eval (AMinus a b) = pure (-) <*> eval a <*> eval b
eval (AMul a b)   = pure (*) <*> eval a <*> eval b
eval (ADiv a b)   = do
  a' <- eval a
  b' <- eval b >>= assertNonZero
  pure (a' `div` b')
eval (AMod a b)   = do
  a' <- eval a
  b' <- eval b >>= assertNonZero
  pure (a' `mod` b')


assertNonZero :: Integer -> ImpExpr Integer
assertNonZero i = case i of
  0 -> failWith "Division by Zero"
  _ -> pure i

-- TODO: Refactor the original `exec` function such that it uses 
-- Either String State instead of State:
exec :: Cmd -> State -> Either String State
exec CSkip st           = pure st
exec (CAssign n v) st   = do
  value <- runExpr (eval v) st
  pure $ M.insert n value st

exec (CIfThEl b th el) st = do
  cond <- runExpr (eval b) st
  if (cond) then (exec th st) else (exec el st)

exec (CWhile b c) st = do
  cond <- runExpr (eval b) st -- Evaluate condition
  if (cond) then do
    st' <- exec c st          -- Run code in while
    exec (CWhile b c) st'     -- Recursively call While command
  else
    pure st

exec (CSeq cs) st = foldM (flip exec) st cs 
