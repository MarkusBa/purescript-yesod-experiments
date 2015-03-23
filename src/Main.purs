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

--decode :: String -> Maybe(M.Map String String)

data Quad = Quad { subject :: String
               , predicate :: String
               , object :: String
               }
{-
instance u22FromJSON :: FromJSON u225 where
    parseJSON (JObject o) = do
        b1 <- o .:  "subject"
        b2 <- o .: "predicate"
        b3 <- o .: "object"
        return $ Quad { subject: b1, predicate: b2, object: b3}
    parseJSON _ = fail "u22 parse failed."
-}


main = runContT (getResponseText purescript_org) $ \response -> do
  let text = decode response :: Maybe String
  case text of
      Just sth -> trace sth
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
