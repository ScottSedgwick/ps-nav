module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, createFactory, spec, createClass)
import React.DOM as D
import ReactDOM (render)

import Data.AddressBook (Person, person, address, phoneNumber, PhoneType(..))
import Data.Model (AppState(..), ViewMode(..))
import View.MainView (renderView)

examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake Street" "FakeTown" "VIC")
         [ phoneNumber HomePhone "03-5555-5555"
         , phoneNumber CellPhone "04-5555-5555"
         ]

initialState :: AppState
initialState = AppState
    { person: examplePerson
    , errors: [] 
    , viewMode: AddressBook
    }

mainView :: forall props. ReactClass props
mainView = createClass $ spec initialState renderView

main :: Eff ( console :: CONSOLE, dom :: DOM ) Unit
main = void do
    log "Rendering address book component"
    let component = D.div [] [ createFactory mainView unit ]
    doc <- window >>= document
    ctr <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
    render component (unsafePartial fromJust ctr)
