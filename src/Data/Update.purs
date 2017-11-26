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
        
valueOf :: Event -> Either (NonEmptyList ForeignError) String
valueOf e = runExcept do
    target <- index (toForeign e) "target"
    value <- index target "value"
    readString value

updatePhoneNumber :: String -> PhoneNumber -> PhoneNumber
updatePhoneNumber s (PhoneNumber o) = PhoneNumber $ o { number = s }
