module Main where

import Prelude
import Pux (EffModel, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick, onSubmit, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Maybe
import Data.Either
import DOM.Event.Event (preventDefault)
import Network.HTTP.Affjax (AJAX, get)
import Network.RemoteData
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML (button, div, form, h1, input, p)
import Text.Smolder.HTML.Attributes (className, placeholder, type')
import Text.Smolder.Markup ((!), (#!), text)


type State =
  { userId :: String
  , user :: RemoteData String User
  }


data Event
  = UserIdChange DOMEvent
  | RequestUser DOMEvent
  | ReceiveUser (Either String User)


newtype User = User
  { id :: Number
  , name :: String
  }


instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    name <- obj .? "name"
    pure $ User { id: id, name: name }

{-- foldp :: forall fx. Event -> State -> EffModel State Event fx --}
foldp (UserIdChange event) state =
  { state: state { userId = targetValue event }
  , effects: []
  }
foldp (RequestUser event) state
  | state.userId == "" = { state: state { user = NotAsked }, effects: [ liftEff (preventDefault event) *> pure Nothing ] }
  | otherwise          = 
    { state: state { user = Loading }
    , effects:
      [ liftEff (preventDefault event) *> pure Nothing
      , do
          res <- attempt $ get ("http://jsonplaceholder.typicode.com/users/" <> state.userId)
          let decode r = decodeJson r.response :: Either String User
          let user = either (Left <<< show) decode res
          pure $ Just $ ReceiveUser user
      ]
    }
foldp (ReceiveUser (Left err)) state =
  { state: state { user = Failure err }
  , effects: []
  }
foldp (ReceiveUser (Right user)) state =
  { state: state { user = Success user }
  , effects: []
  }

view :: State -> HTML Event
view state =
  div ! className "flex flex-column helvetica items-center justify-center min-vh-100" $ do
    h1 ! className "blue f1 ttu" $ text "First Network Request"
    form ! className "flex flex-column w-90" #! onSubmit RequestUser $ do
      input ! type' "number" ! placeholder "user id" ! className "outline-0 bb-1 bl-0 br-0 bt-0 f4 mb2" #! onChange UserIdChange
      button ! type' "submit" ! className "bg-light-blue bn f4 hover-bg-blue outline-0 pa2 pointer white" $ text "Load Users"
    viewUser state.user

{-- viewUser :: Maybe User -> HTML Event --}
viewUser :: RemoteData String User -> HTML Event
viewUser user = p ! className "f2" $ text content
  where
    content =
      case user of
        NotAsked -> "search for a user!"
        Loading -> "searching..."
        Failure _ -> "hhmmm... looks like we couldn’t find that user :("
        Success (User user) -> user.name
      

init :: State
init =
  { userId: ""
  , user: NotAsked
  }


{-- main :: forall fx. Eff (channel :: CHANNEL, exception :: EXCEPTION | fx) Unit --}
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }
  renderToDOM "#app" app.markup app.input

































