module View.MainView (mainView) where

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..))
import Data.Array ((..), length, zipWith)
import Data.Model (AppState(..), Model, ViewMode(..))
import Data.Update (update, Action(..))
import Prelude (bind, map, pure, show, ($), (<>))
import React (ReactClass, ReactElement, createClass, readState, spec)
import React.DOM as D
import React.DOM.Props as P
import Style.Bootstrap ((<+>))
import Style.Bootstrap as BS

import View.Types (ReactElementEff, UpdateHandler)

mainView :: forall props. AppState -> ReactClass props
mainView initialState = createClass $ spec initialState renderView

renderView :: forall props eff. Model props -> ReactElementEff eff
renderView ctx = do
    -- Read State
    AppState { person: Person p@{ homeAddress: Address a }, errors: e, viewMode: v } <- readState ctx
    let pers = Person p
    let addr = Address a
    pure $ do
        D.div [ P.className BS.style.container ]
            [ renderToolbar ctx -- Render toolbar
            , case v of         -- Render focused page
                AddressBook -> renderAddressBook ctx pers addr e
                Other -> renderOther ctx
            ]

renderToolbar :: forall props. Model props -> ReactElement
renderToolbar ctx =
    D.div [ P.className BS.style.container ]
        [ D.button [ P.onClick (update ctx (ChangeView AddressBook))] [ D.text "AddressBook" ]
        , D.button [ P.onClick (update ctx (ChangeView Other))] [ D.text "Other" ]
        ]

renderOther :: forall props. Model props -> ReactElement
renderOther ctx = D.div [ P.className BS.style.container ]
                    [ D.div [ P.className BS.style.row ]
                        [ D.label [ P.className $ BS.style.colSm2 <+> BS.style.controlLabel ] [ D.text "What the F am I doing?" ]
                        ]
                    ]

renderValidationError :: String -> ReactElement
renderValidationError err = D.li' [ D.text err ]

renderValidationErrors :: Array String -> Array ReactElement
renderValidationErrors [] = []
renderValidationErrors xs = [ D.div [ P.className $ BS.style.alert <+> BS.style.alertDanger ] [ D.ul' (map renderValidationError xs) ] ]

renderField :: forall eff. String -> String -> String -> UpdateHandler eff -> ReactElement
renderField name hint value update =
    D.div [ P.className BS.style.formGroup ]
          [ D.label [ P.className $ BS.style.colSm2 <+> BS.style.controlLabel ] [ D.text name ]
          , D.div [ P.className BS.style.colSm3 ]
                  [ D.input [ P._type "text"
                            , P.className BS.style.formControl
                            , P.placeholder hint
                            , P.value value
                            , P.onChange update
                            ] []
                  ]
          ]


renderPhoneNumber :: forall props. Model props -> Person -> PhoneNumber -> Int -> ReactElement
renderPhoneNumber ctx (Person person) (PhoneNumber phone) index =
    renderField (show phone."type") "XXX-XXX-XXXX" phone.number (update ctx (ChangePhoneNumber index))

renderAddressBook :: forall props. Model props -> Person -> Address -> Array String -> ReactElement
renderAddressBook ctx (Person p) (Address a) e = do
    let pers = Person p
    let addr = Address a
    D.div [ P.className BS.style.container ]
        [ D.div [ P.className BS.style.row ]
                [ D.form [ P.className BS.style.formHorizontal ] $
                        [ D.h3' [ D.text "Basic Information" ]

                        , renderField "First Name" "First Name" p.firstName (update ctx ChangeFirstName)
                        , renderField "Last Name"  "Last Name"  p.lastName  (update ctx ChangeLastName)

                        , D.h3' [ D.text "Address" ]

                        , renderField "Street" "Street" a.street (update ctx ChangeStreet)
                        , renderField "City"   "City"   a.city   (update ctx ChangeCity)
                        , renderField "State"  "State"  a.state  (update ctx ChangeState)

                        , D.h3' [ D.text "Contact Information" ]
                        ]
                        <> zipWith (renderPhoneNumber ctx pers) p.phones (0 .. length p.phones)
                ]
        , D.div [ P.className BS.style.row ] (renderValidationErrors e)
        ]