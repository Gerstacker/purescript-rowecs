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

uni0=allocateStorageUniform u (Proxy2 :: Proxy2 IntMap)


--t = CompStorage $ set (unCompStorage v).b 4 "ffuu"

uni1 = write uni0 (SProxy::SProxy "c") 13 7.5
uni2 = write uni1 (SProxy::SProxy "b") 12 "asdf"
uni3 = write uni2 (SProxy::SProxy "a") 13 44

rw = RProxy :: RProxy (c::Number, a::Int)

rs = readStorage uni3 13 :: Record (c::Number, a::Int)

het0 = allocateStorage (RProxy :: RProxy (a:: IntMap Int, b::IntMap String, c:: IntMap Number))
het1 = write het0 (SProxy::SProxy "c") 55 2.3

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow (unCS uni0).b
  logShow (unCS uni1).c
  logShow $ read uni1 (SProxy::SProxy "c") 13
  logShow $ read uni2 (SProxy::SProxy "c") 13
  logShow $ read uni2 (SProxy::SProxy "c") 10
  logShow $ read uni2 (SProxy::SProxy "b") 12
  logShow $ rs.a
  logShow $ rs.c
  logShow $ ((readStorage het1 55) :: Record (c::Number)).c
