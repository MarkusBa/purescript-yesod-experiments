module Main where

import Data.Array (map)
import Data.String (joinWith, length)
import Control.Monad.Eff
import Control.Monad.Cont.Trans
import Network.HTTP.Client
import Debug.Trace
import Data.JSON
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Eff.DOM

{-
data Quad = Quad { subject :: String
               , predicate :: String
               , object :: String
               }
instance u22FromJSON :: FromJSON u225 where
    parseJSON (JObject o) = do
        b1 <- o .:  "subject"
        b2 <- o .: "predicate"
        b3 <- o .: "object"
        return $ Quad { subject: b1, predicate: b2, object: b3}
    parseJSON _ = fail "u22 parse failed."
-}

--Cannot unify Control.Monad.Eff.DOM.Node with Prelude.Unit.
--updateUI :: Maybe (M.Map String String) -> forall eff. Eff (dom :: Control.Monad.Eff.DOM.DOM | eff) Unit
updateUI json = do
    Just container <- querySelector ".container"
    setInnerHTML "Test" container

main = runContT (getResponseText purescript_org) $ \response -> do
  let text = decode response :: Maybe (M.Map String String)
  updateUI text
  case text of
      Just sth -> trace (M.showTree sth)
      Nothing -> trace "nothing"

  where
  getResponseText req = responseToString <$> getAll req

  responseToString :: Response -> String
  responseToString (Response chunks) = joinWith "" $ map runChunk chunks
  
  purescript_org :: Request
  purescript_org = Request 
    { host: "localhost"
    , port: "3000"
    , path: "/quadrest/1" 
    }
