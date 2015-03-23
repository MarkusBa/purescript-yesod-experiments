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

main = trace "Hi"
--main = runContT (decodeText purescript_org) trace
{-
main = runContT (getResponseText purescript_org) trace
-}
  where
  decodeText req = decode <$> getResponseText req
  getResponseText req = responseToString <$> getAll req

  responseToString :: Response -> String
  responseToString (Response chunks) = joinWith "" $ map runChunk chunks
  
  purescript_org :: Request
  purescript_org = Request 
    { host: "localhost"
    , port: "3000"
    , path: "/quadrest/1" 
    }
