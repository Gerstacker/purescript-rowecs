module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.IntMap (IntMap, empty)
--import Data.Record.Extra (mapRecord, class MapRecord)
--import Type.Prelude (class RowToList)
import Type.Proxy (Proxy2(Proxy2))
import Type.Prelude(RProxy(RProxy), SProxy(SProxy))
import ECS


u = RProxy :: RProxy (a::Int, b::String, c::Number)

v=allocateStorageUniform u (Proxy2 :: Proxy2 IntMap)


--t = CompStorage $ set (unCompStorage v).b 4 "ffuu"

w = write v (SProxy::SProxy "c") 13 7.5
q = write w (SProxy::SProxy "b") 12 "asdf"
z = write q (SProxy::SProxy "a") 13 44

rw = RProxy :: RProxy (c::Number, a::Int)

rs = readStorage z 13 :: Record (c::Number, a::Int)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow (unCS v).b
  logShow (unCS w).c
  logShow $ read w (SProxy::SProxy "c") 13
  logShow $ read q (SProxy::SProxy "c") 13
  logShow $ read q (SProxy::SProxy "c") 10
  logShow $ read q (SProxy::SProxy "b") 12
  logShow $ rs.a
  logShow $ rs.c
