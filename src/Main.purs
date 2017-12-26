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


-- g :: forall a . Proxy a -> IntMap a
-- g _ = empty

-- makeWorld :: forall row row' xs a . RowToList row xs => MapRecord xs row (Proxy a) (IntMap a) row' => { | row } -> { | row' }
-- makeWorld a = mapRecord g a

u = RProxy :: RProxy (a::Int, b::String, c::Number)

v=allocateStorage u (Proxy2 :: Proxy2 IntMap)


t = set v.b 4 "ffuu"

w = write v (SProxy::SProxy "c") 13 7.5
q = write w (SProxy::SProxy "b") 12 "asdf"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  logShow v.b
  logShow t
  logShow w.c
  logShow $ read w (SProxy::SProxy "c") 13
  logShow $ read q (SProxy::SProxy "c") 13
  logShow $ read q (SProxy::SProxy "c") 10
  logShow $ read q (SProxy::SProxy "b") 12
