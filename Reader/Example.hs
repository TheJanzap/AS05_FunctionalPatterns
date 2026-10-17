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


-- This function runs a ConfigReader action with a given config.
runWithConfig :: ConfigReader a -> Config -> a
runWithConfig (ConfigReader r) config = r config


-- This function allows to access the Config.
getConfig :: ConfigReader Config
getConfig = ConfigReader id


-- Here is the Functor instance:
instance Functor ConfigReader where
  fmap :: (a -> b) -> ConfigReader a -> ConfigReader b
  fmap f (ConfigReader g) = ConfigReader (\c -> f (g c)) -- Same as ConfigReader (f.g)


-- This is the Applicative instance:
instance Applicative ConfigReader where
  pure :: a -> ConfigReader a
  pure a = ConfigReader (\_ -> a)

  (<*>) :: ConfigReader (a -> b) -> ConfigReader a -> ConfigReader b
  (ConfigReader f) <*> (ConfigReader a) = ConfigReader (\c -> f c (a c))


-- Here is the Monad instance:
instance Monad ConfigReader where
  (>>=) :: ConfigReader a -> (a -> ConfigReader b) -> ConfigReader b
  (ConfigReader a) >>= f = ConfigReader (\c -> runWithConfig (f (a c)) c)   


-- Here are the three functions again, but this time they return a ConfigReader.
-- They should behave exactly as the original functions defined above.

--createBody is already implemented as a reference.
createBody :: String -> ConfigReader String
createBody addr = do
  config <- getConfig
  pure $ "mailto:" ++ addr ++ "\n" ++ if devStage config then "Test" else "Dear .."


-- TODO: Implement createAddr using do-notation.
createAddr :: String -> ConfigReader String
createAddr name = do
  config <- getConfig
  pure $ name ++ if devStage config then "@dummy.com" else "@fp.io"


-- TODO: Implement createMail using do-notation.
createMail :: String -> ConfigReader String
createMail name = do
  config <- getConfig
  pure $ createBody' (createAddr' name config) config

-- Here is how we can run a ConfigReader action with a given config:
-- Expected value: "mailto:daniel@fp.io\nDear .."
testReader :: String
testReader = runWithConfig (createMail "daniel") (Config False)
