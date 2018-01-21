module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.IntMap (IntMap, empty, fromAssocArray)
--import Data.Record.Extra (mapRecord, class MapRecord)
--import Type.Prelude (class RowToList)
import Type.Proxy (Proxy2(Proxy2))
import Type.Prelude(RProxy(RProxy), SProxy(SProxy))
import Data.Tuple
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

arrA = [ (Tuple 1 17), (Tuple 2 137), (Tuple 5 11)]
arrB = [ (Tuple 1 "Enoch"), (Tuple 5 "Cassandra")]
arrC = [ (Tuple 2 44.1), (Tuple 5 16.3)]

cs = CompStorage { a: fromAssocArray arrA, b: fromAssocArray arrB, c: fromAssocArray arrC}
rpa = RProxy :: RProxy (c :: Number, b :: String)

fn1 :: Record (a :: Int, b :: String) -> Record (b :: String)
fn1 r = { b : (show r.a) <> r.b }


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow (unCS uni0).b
  logShow (unCS uni1).c
  logShow $ read uni1 (SProxy::SProxy "c") 13
  logShow $ read uni2 (SProxy::SProxy "c") 13
  logShow $ read uni2 (SProxy::SProxy "c") 10
  logShow $ intersectIndices cs rpa
  logShow $ (applyFn cs fn1 5).b
