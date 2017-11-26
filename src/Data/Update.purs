module Data.Update where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExcept)
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..))
import Data.AddressBook.Validation (validatePerson')
import Data.Array (modifyAt)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign (ForeignError, readString, toForeign)
import Data.Foreign.Index (index)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromMaybe)
import Data.Model (AppState(..), Model, ViewMode)
import React (ReadWrite, ReactState, Event, readState, writeState)

data Action = ChangeView ViewMode
            | ChangeFirstName
            | ChangeLastName
            | ChangeStreet
            | ChangeCity
            | ChangeState
            | ChangePhoneNumber Int

update :: forall props eff. Model props -> Action -> Event -> Eff ( console :: CONSOLE, state :: ReactState ReadWrite | eff ) Unit
update ctx act evt = for_ (valueOf evt) \s -> do
    -- Read current state
    AppState { person: Person p@{ homeAddress: Address a }, errors: e, viewMode: v } <- readState ctx
    -- Modify according to the action
    let ns = case act of
                ChangeView viewMode     -> AppState { person: Person p, errors: e, viewMode: viewMode }
                ChangeFirstName         -> AppState { person: Person (p { firstName = s }), errors: e, viewMode: v }
                ChangeLastName          -> AppState { person: Person (p { lastName = s }), errors: e, viewMode: v }
                ChangeStreet            -> AppState { person: Person $ p { homeAddress = Address $ a { street = s } }, errors: e, viewMode: v}
                ChangeCity              -> AppState { person: Person $ p { homeAddress = Address $ a { city = s } }, errors: e, viewMode: v}
                ChangeState             -> AppState { person: Person $ p { homeAddress = Address $ a { state = s } }, errors: e, viewMode: v}
                ChangePhoneNumber index -> AppState { person: Person $ p { phones = fromMaybe p.phones $ modifyAt index (updatePhoneNumber s) p.phones }, errors: e, viewMode: v}
    -- Run validators here
    let AppState { person: np, errors: _, viewMode: nv} = ns
    let nns = case validatePerson' np of
                Left es -> AppState { person: np, errors: es, viewMode: nv }
                Right _ -> AppState { person: np, errors: [], viewMode: nv }
    -- Write changed state
    writeState ctx nns
        
  
-- changeModel :: forall props eff. Model props -> Person -> Errors -> Eff ( console :: CONSOLE, state :: ReactState ReadWrite | eff ) AppState
-- changeModel ctx p e = writeState ctx (AppState { person: p, errors: e, viewMode: AddressBook })

-- changeViewMode :: forall props eff. Model props -> ViewMode -> Event -> Eff ( state :: ReactState ReadWrite | eff ) AppState
-- changeViewMode ctx nv _ = do
--     AppState { person: p, errors: e, viewMode: _ } <- readState ctx
--     writeState ctx (AppState { person: p, errors: e, viewMode: nv })

valueOf :: Event -> Either (NonEmptyList ForeignError) String
valueOf e = runExcept do
    target <- index (toForeign e) "target"
    value <- index target "value"
    readString value

-- type AppStateUpdate a = (AppState -> a -> AppState)

-- updateAppState :: forall props eff a. Model props -> (String -> Person) -> Event -> Eff ( console :: CONSOLE, state :: ReactState ReadWrite | eff ) Unit
-- updateAppState ctx update e =
--     for_ (valueOf e) \s -> do
--         let newPerson = update s
--         log "Running validators"
--         case validatePerson' newPerson of
--             Left errors -> changeModel ctx newPerson errors
--             Right _     -> changeModel ctx newPerson [] 

-- updateFN :: AppStateUpdate String
-- updateFN (AppState { person: (Person p), errors: e, viewMode: v }) s =
--     AppState { person: Person (p {firstName = s}), errors: e, viewMode: v }

-- updateFirstName :: Person -> String -> Person
-- updateFirstName (Person p) s = Person $ p { firstName = s }

-- updateLastName :: Person -> String -> Person
-- updateLastName (Person p) s = Person $ p { lastName  = s }

-- updateStreet :: Person -> Address -> String -> Person
-- updateStreet (Person p) (Address a) s = Person $ p { homeAddress = Address $ a { street = s } }

-- updateCity :: Person -> Address -> String -> Person
-- updateCity (Person p) (Address a) s = Person $ p { homeAddress = Address $ a { city   = s } }

-- updateState :: Person -> Address -> String -> Person
-- updateState (Person p) (Address a) s = Person $ p { homeAddress = Address $ a { state  = s } }

updatePhoneNumber :: String -> PhoneNumber -> PhoneNumber
updatePhoneNumber s (PhoneNumber o) = PhoneNumber $ o { number = s }
