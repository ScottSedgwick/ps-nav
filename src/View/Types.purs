module View.Types where
  
import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import React (ReactState, ReactProps, ReactRefs, Event, ReadOnly, ReadWrite, ReactElement)

type ReactElementEff eff = Eff (state :: ReactState ReadOnly | eff) ReactElement

type UpdateHandler eff = Event -> Eff ( props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff ) Unit