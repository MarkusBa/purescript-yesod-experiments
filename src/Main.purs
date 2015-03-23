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

{-
commands:
pulp dep update
pulp browserify --to Main.js
or pulp run

-}

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

quadS (Quad {subject = subject, predicate = predicate, object = object}) =
  subject
quadP (Quad {subject = subject, predicate = predicate, object = object}) =
  predicate
quadO (Quad {subject = subject, predicate = predicate, object = object}) =
  object

updateUI quadM = do
    Just container <- querySelector ".container"

    let quad = case quadM of
         Just quadI -> quadI
         Nothing -> Quad {subject: "s", predicate: "p", object: "o"}
    ul <- createElement "ul"

    let sub = quadS quad
    let pre = quadP quad
    let obj = quadO quad

    li <- createElement "li" >>= setText sub
    li `appendChild` ul
    li2 <- createElement "li" >>= setText pre
    li2 `appendChild` ul      
    li3 <- createElement "li" >>= setText obj
    li3 `appendChild` ul

    ul `appendChild` container
 

main = runContT (getResponseText purescript_org) $ \response -> do
  let quad = decode response :: Maybe Quad
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
