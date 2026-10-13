-------------------------------------------------------------------------------
-- Part 3: Applicative Validation
-------------------------------------------------------------------------------

-- In this exercise, you will implement a validation "framework" and 
-- integrate it into a small REST service. The REST service allows users 
-- to create new orders (POST) and to retrieve all existing orders (GET).

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import           Web.Scotty
import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.Text.Lazy as L
import           Network.HTTP.Types (badRequest400)

todo :: a
todo = error "TODO"


-- In the example below, we validate orders based on the customer's email address 
-- and the number of items they want.
-- The data comes as an OrderJSON record from the REST service:
data OrderJSON = OrderJSON {
  jsonEmail :: String,
  jsonAmount :: Int
} deriving (Eq, Show)

-- We want to validate this data. Email addresses must follow a valid format, 
-- and the quantity should be greater than 0. To clearly distinguish 
-- between uncontrolled and validated values, we define new types for 
-- valid data:

-- Newtypes for Email and Amount:
newtype Email = Email String deriving (Eq, Show, ToJSON)
newtype Amount = Amount Int deriving (Eq, Show, ToJSON)

-- A type for validated orders:
data Order = Order {
  orderEmail :: Email,
  orderAmount :: Amount 
} deriving (Eq, Show)


-------------------------------------------------------------------------------
-- Validation using `Either String a` for error handling
-------------------------------------------------------------------------------

-- Now the code to validate email addresses:
validateEmail' :: String -> Either String Email
validateEmail' email 
    | '@' `elem` email = Right (Email email)
    | otherwise        = Left "Illegal email format."

-- TODO: Check the values test1 and test2:
test1, test2 :: Either String Email
test1 = validateEmail' "student@fhnw.ch"
test2 = validateEmail' "student$$$fhnw.ch"

-- This works. The function for validating the quantity is similar:
validateAmount' :: Int -> Either String Amount
validateAmount' amount 
    | amount > 0 = Right (Amount amount)
    | otherwise  = Left "Amount must be positive."

-- The implementation above should work as expected. Now we want to combine 
-- these two validations to validate an OrderJSON into an Order:
validateOrder' :: OrderJSON -> Either String Order
validateOrder' orderJson = pure Order <*> vEmail <*> vAmount
   where 
       vEmail = validateEmail'   $ jsonEmail orderJson
       vAmount = validateAmount' $ jsonAmount orderJson

-- The above implementation uses the Applicative instance for the 
-- (Either String) data type. 
-- TODO: Check the values test3, test4 and test5:
test3, test4, test5, test6 :: Either String Order
test3 = validateOrder' (OrderJSON "student@fhnw.ch" 3)
test4 = validateOrder' (OrderJSON "student$$$fhnw.ch" 3)
test5 = validateOrder' (OrderJSON "student@fhnw.ch" (-1))

-- This works! Unfortunately, we only get the first validation error. 
-- In the following example, we would like to get both errors to display 
-- all messages to the user who entered the data in a web form:
test6 = validateOrder' (OrderJSON "student$$$fhnw.ch" (-1))

-- The issue arises from the Applicative instance for the Either type.
-- It's defined as follows:
--
-- instance Applicative (Either e) where
--     pure          = Right
--     Left  e <*> _ = Left e
--     Right f <*> r = fmap f r
--
-- If the first argument is a Left, the second one is ignored.


-------------------------------------------------------------------------------
-- Validation which collects multiple errors
-------------------------------------------------------------------------------

-- Instead of just getting the first error, we want to collect all errors.
-- Therefore we need a different Applicative instance and thus we also need 
-- our own type:
newtype Validation e r = Validation (Either e r) deriving (Eq, Show)

-- TODO: Define a Functor instance:
instance Functor (Validation e) where
  fmap :: (a -> b) -> Validation e a -> Validation e b
  fmap f (Validation v) = Validation (fmap f v) -- Calling fmap on `Either` to handle `Left` and `Right` 

-- To collect all errors, we constrain the error type `e` to be an 
-- instance of Monoid.
-- If (<*>) encounters a Left on both sides, it should produce a Left 
-- whose values are combined with (<>).
-- In other cases, you can delegate to the Applicative instance 
-- of the wrapped Either.
-- TODO: Define an Applicative instance:
instance (Monoid e) => Applicative (Validation e) where
  pure :: a -> Validation e a
  pure a = Validation (Right a) -- Create a validated value, no error

  (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
  -- Special Case: Combine two errors with each other
  (<*>) (Validation (Left e1)) (Validation (Left e2)) = Validation (Left (e1 <> e2))
  -- General Case: Combine two values or a value with an error
  (<*>) (Validation l) (Validation r) = Validation (l <*> r)


-- To collect errors, we can now equip Validation with a monoidal 
-- type for the error message. Here we use [String] to collect errors:
validateEmail :: String -> Validation [String] Email
validateEmail email 
    | '@' `elem` email = Validation $ Right (Email email)
    | otherwise        = Validation $ Left ["Illegal email format."]


validateAmount :: Int -> Validation [String] Amount
validateAmount amount 
    | amount > 0 = Validation $ Right (Amount amount)
    | otherwise  = Validation $ Left ["Amount must be positive."]

   
validateOrder :: OrderJSON -> Validation [String] Order
validateOrder orderJson = Order <$> vEmail <*> vAmount
    where
         vEmail = validateEmail   $ jsonEmail orderJson
         vAmount = validateAmount $ jsonAmount orderJson

-- The following output should now contain both validation errors.
-- Expected output: Validation (Left ["Illegal email format.","Amount must be positive."])
-- TODO: Inspect the value of test. 
test :: Validation [String] Order
test = validateOrder (OrderJSON "student$$$fhnw.ch" (-1))


-------------------------------------------------------------------------------
-- REST service with validation
-------------------------------------------------------------------------------

-- JSON parsing for OrderJSON
instance FromJSON OrderJSON where
  parseJSON = withObject "Order" $ \o -> 
    pure OrderJSON  <*> o .: "email" <*> o .: "amount"  -- <- APPLICATIVE :)

-- JSON serialization for Order
instance ToJSON Order where
  toJSON (Order m a) = object ["email" .=  m, "amount" .= a]


main :: IO ()
main = do
  -- Mutable reference to all validated orders
  orders <- newMVar [Order (Email "daniel.kroeni@fhnw.ch") (Amount 12)] 
  scotty 3000 $ do
    get "/order" $ do
      os <- liftIO $ readMVar orders
      json os 
    post "/order" $ do
      jsonOrder <- jsonData
      case validateOrder jsonOrder of
        (Validation (Left msgs)) -> do
          status badRequest400 
          json $ map L.pack msgs
        (Validation (Right order)) -> do
          liftIO $ modifyMVar_ orders $ (\os -> return (order:os))
          redirect "/order"
