module Data.Model where
  
import Data.AddressBook (Person)
import Data.AddressBook.Validation (Errors)
import React (ReactThis)

data ViewMode = AddressBook | Other

newtype AppState = AppState
    { person :: Person
    , errors :: Errors
    , viewMode :: ViewMode
    }

type Model props = ReactThis props AppState