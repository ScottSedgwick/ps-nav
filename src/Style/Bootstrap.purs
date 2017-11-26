module Style.Bootstrap (style, addWithSpace, (<+>)) where

import Prelude 
  
type BootstrapStyles = 
    { alert :: String
    , alertDanger :: String
    , colSm2 :: String
    , colSm3 :: String
    , container :: String
    , controlLabel :: String
    , formControl :: String
    , formGroup :: String
    , formHorizontal :: String
    , row :: String
    }

style :: BootstrapStyles
style = 
    { alert: "alert"
    , alertDanger: "alert-danger"
    , colSm2: "col-sm-2"
    , colSm3: "col-sm-3"
    , container: "container"
    , controlLabel: "control-label"
    , formControl: "form-control"
    , formGroup: "form-group"
    , formHorizontal: "form-horizontal"
    , row: "row"
    }

addWithSpace :: String -> String -> String
addWithSpace x y = x <> " " <> y

infixr 5 addWithSpace as <+>