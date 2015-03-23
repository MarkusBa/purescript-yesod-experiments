module Main where

import Data.Array (map, null)
import Data.String (joinWith, length)
import Control.Monad.Eff
import Control.Monad.Cont.Trans
import Network.HTTP.Client
import Debug.Trace
import Data.JSON
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Eff.DOM
import Data.Array.Unsafe (tail)

data Quad = Quad { subject :: String
               , predicate :: String
               , object :: String
               }
               
instance quadFromJSON :: FromJSON Quad where
    parseJSON (JObject o) = do
        b1 <- o .:  "subject"
        b2 <- o .: "predicate"
        b3 <- o .: "object"
        return $ Quad { subject: b1, predicate: b2, object: b3}
    parseJSON _ = fail "quad parse failed."

instance quadToJSON :: ToJSON Quad where
    toJSON (Quad { subject = b1, predicate = b2, object = b3 }) =
        object ["subject" .= b1, "predicate" .= b2, "object" .= b3]

lengthl :: forall a. [a] -> Number
lengthl arr =
    if null arr
    then 0
    else 1 + lengthl (tail arr)

--Cannot unify Control.Monad.Eff.DOM.Node with Prelude.Unit.
--updateUI :: Maybe (M.Map String String)  -> forall eff. Eff (dom :: Control.Monad.Eff.DOM.DOM | eff) Unit
updateUI quad = do
    Just container <- querySelector ".container"

    let list = case quad of
         Just quad -> M.values quad
         Nothing -> []
    ul <- createElement "ul"
    foreachE list $ \element -> do
      li <- createElement "li" >>= setText element
      li `appendChild` ul
      return unit

    ul `appendChild` container
 

main = runContT (getResponseText purescript_org) $ \response -> do
  let quad = decode response :: Maybe (M.Map String String)
  updateUI quad
  trace "finished manipulating ui"

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
