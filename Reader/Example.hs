-- In this exercise you will refactor some code which explicitly 
-- passes configuration data around into a program threads the
-- configuration data through implicitly using a Reader Monad.

{-# LANGUAGE InstanceSigs #-}
module Reader.Example where
import Prelude hiding (Maybe(..), lookup)

todo :: a
todo = error "TODO"

-- The configuration data for our application contains a Bool stating 
-- whether or not we're in development mode.                  
newtype Config = Config { devStage :: Bool }


-- This function takes a name and a config and depending on the config
-- it returns a dummy email address or a real one.
createAddr' :: String -> Config -> String
createAddr' name config =
  name ++ if devStage config then "@dummy.com" else "@fp.io"


-- This function takes an email address and a config and depending on
-- the config it returns a dummy email body or a real one.
createBody' :: String -> Config -> String
createBody' addr config =
   "mailto:" ++ addr ++ "\n" ++ if devStage config then "Test" else "Dear .."


-- This function takes a name and a config and depending on the config
-- it returns a dummy email or a real one.
createMail' :: String -> Config -> String
createMail' name config = createBody' (createAddr' name config) config


-- Expected value: "mailto:daniel@fp.io\nDear .."
test :: String
test = createMail' "daniel" (Config False)


-- Observe that the Config parameter is passed to the three functions
-- createAddr', createBody' and createMail' as the last parameter 
-- before the return type.


-------------------------------------------------------------------------------
-- Enter the Reader Monad
-------------------------------------------------------------------------------

-- Our goal is to get rid of passing the Config parameter to all functions 
-- explicitly by refactoring the code such that the functions return a value 
-- of type `ConfigReader a` instead of `Config -> a`.
newtype ConfigReader a = ConfigReader (Config -> a)

-- Optional task
newtype Reader env a = Reader (env -> a)


-- This function runs a ConfigReader action with a given config.
runWithConfig :: ConfigReader a -> Config -> a
runWithConfig (ConfigReader r) config = r config

runInEnv :: Reader env a -> env -> a
runInEnv (Reader f) env = f env


-- This function allows to access the Config.
getConfig :: ConfigReader Config
getConfig = ConfigReader id

getConfigEnv :: (Reader env) env
getConfigEnv = Reader id

-- Here is the Functor instance:
instance Functor ConfigReader where
  fmap :: (a -> b) -> ConfigReader a -> ConfigReader b
  fmap f (ConfigReader g) = ConfigReader (\c -> f (g c)) -- Same as ConfigReader (f.g)

instance Functor (Reader env) where
  fmap :: (a -> b) -> Reader env a -> Reader env b
  fmap f (Reader r) = Reader (\env -> f (r env))


-- This is the Applicative instance:
instance Applicative ConfigReader where
  pure :: a -> ConfigReader a
  pure a = ConfigReader (\_ -> a)

  (<*>) :: ConfigReader (a -> b) -> ConfigReader a -> ConfigReader b
  (ConfigReader fl) <*> (ConfigReader fr) = ConfigReader (\c -> fl c (fr c))

instance Applicative (Reader env) where
  pure :: a -> Reader env a
  pure a = Reader (\_ -> a)

  (<*>) :: Reader env (a -> b) -> Reader env a -> Reader env b
  (Reader fl) <*> (Reader fr) = Reader (\env -> fl env (fr env))


-- Here is the Monad instance:
instance Monad ConfigReader where
  (>>=) :: ConfigReader a -> (a -> ConfigReader b) -> ConfigReader b
  (ConfigReader a) >>= f = ConfigReader (\c -> runWithConfig (f (a c)) c)   


instance Monad (Reader env) where
  (>>=) :: Reader env a -> (a -> Reader env b) -> Reader env b
  (Reader a) >>= f = Reader (\env -> runInEnv (f (a env)) env)


-- Here are the three functions again, but this time they return a ConfigReader.
-- They should behave exactly as the original functions defined above.

--createBody is already implemented as a reference.
createBody :: String -> ConfigReader String
createBody addr = do
  config <- getConfig
  pure $ "mailto:" ++ addr ++ "\n" ++ if devStage config then "Test" else "Dear .."

createBody'' :: String -> (Reader Config) String
createBody'' addr = do
  config <- getConfigEnv
  pure $ "mailto:" ++ addr ++ "\n" ++ if devStage config then "Test" else "Dear .."


-- TODO: Implement createAddr using do-notation.
createAddr :: String -> ConfigReader String
createAddr name = do
  config <- getConfig
  pure $ name ++ if devStage config then "@dummy.com" else "@fp.io"

createAddr'' :: String -> (Reader Config) String
createAddr'' name = do
  config <- getConfigEnv
  pure $ name ++ if devStage config then "@dummy.com" else "@fp.io"


-- TODO: Implement createMail using do-notation.
createMail :: String -> ConfigReader String
createMail name = do
  config <- getConfig
  pure $ createBody' (createAddr' name config) config

createMail'' :: String -> (Reader Config) String
createMail'' name = do
  addr <- createAddr'' name
  createBody'' addr


-- Here is how we can run a ConfigReader action with a given config:
-- Expected value: "mailto:daniel@fp.io\nDear .."
testReader :: String
testReader = runWithConfig (createMail "daniel") (Config False)

testReader' :: String
testReader' = runInEnv (createMail'' "daniel") (Config False)