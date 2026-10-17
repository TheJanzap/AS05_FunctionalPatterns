{-# LANGUAGE InstanceSigs #-}
module Data.Option where
import Prelude hiding (lookup)

todo :: a
todo = error "TODO"

-- An `Option a` models a value of type `a` which may be missing.
data Option a = None | Some a deriving (Show, Eq)

-- Now implement Functor, Applicative and Monad
instance Functor Option where
  fmap :: (a -> b) -> Option a -> Option b
-- Part 1
--    fmap _ None     = None
--    fmap f (Some a) = Some (f a)

-- Part 2
-- `pure f` wraps the function in Option, returns a applicative function:
-- Either `f (Option a)` for `None` or `f (a -> Option a)` for `Some`
-- `<*>` applies this function to `a` and returns either `None` or `Some a`
-- Note that we don't need to handle the none case, `<*>` handles it for us
  fmap f a = pure f <*> a

instance Applicative Option where
    pure :: a -> Option a
    pure a = Some a

    (<*>) :: Option (a -> b) -> Option a -> Option b
    -- Part 1
    -- (<*>) None _             = None 
    -- (<*>) _ None             = None
    -- (<*>) (Some f) (Some a)  = Some (f a)
    
    -- Part 2
    (<*>) mf ma =
      mf >>= \f ->    -- Unpack the function from its context (e.g. Some (a -> b) -> (a -> b))
        ma >>= \a ->  -- Unpack `a` from its context (e.g. Some (a) -> a)
          pure (f a)  -- Apply `f` on `a` and wrap it back into its context
-- Again, no need to handle the `None` case, `>>=` will return automatically. 



instance Monad Option where
  (>>=) :: Option a -> (a -> Option b) -> Option b
  None     >>= _ = None
  (Some a) >>= f = f a

-- TODO: Check your definitions by running the tests:
-- > cabal test option --test-show-details=direct


-------------------------------------------------------------------------------
-- Example: Bank accounts
-------------------------------------------------------------------------------

data Account = MkAccount { userId :: Int, balance :: Double }

users :: [(String, Int)]
users = [
  ("daniel.kroeni@fhnw.ch", 42),
  ("hans@noaccou.nt", 43)]

accounts :: [Account]
accounts = [
 MkAccount 1 5,
 MkAccount 41 20,
 MkAccount 42 0]


lookup :: Eq a => a -> [(a,b)] -> Option b
lookup _ [] = None
lookup key ((k,v):kvs) 
  | key == k = Some v
  | otherwise = lookup key kvs


userIdByEmail :: String -> Option Int
userIdByEmail email = lookup email users 


accountByUserId :: Int -> Option Account
accountByUserId uid = lookup uid accountKVs 
  where accountKVs = map (\a -> (userId a, a)) accounts


balanceByEmail' :: String -> Option Double
balanceByEmail' email =
  case userIdByEmail email of
    None -> None
    Some uid -> case accountByUserId uid of
      None -> None
      Some acc -> Some (balance acc)

-- TODO: Implement the same functionality as above using `>>=` 
-- instead of pattern matching:
balanceByEmail'' :: String -> Option Double
balanceByEmail'' email = userIdByEmail email >>= \uid ->
  accountByUserId uid >>= \account ->
    pure (balance account)

-- TODO: Implement the same functionality as above using do-notation:
balanceByEmail :: String -> Option Double
balanceByEmail email = do
  uid <- userIdByEmail email
  account <- accountByUserId uid
  pure (balance account)

main :: IO ()
main = do
  print $ balanceByEmail "daniel.kroeni@fhnw.ch"
