module Main where

import ECS

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Data.IntMap (IntMap, fromAssocArray)
import Data.Record.Extra (type (:::))
import Data.Tuple (Tuple(Tuple))
import Prelude (Unit, discard, show, ($), (<>))
import Type.Prelude (RProxy(RProxy), SProxy(SProxy))
import Data.Semiring ((+), (*))
import Type.Proxy (Proxy2(Proxy2))
import Data.String (length)
import Data.Int (toNumber)


u = RProxy :: RProxy (a::Int, b::String, c::Number)

type SRow = (a::IntMap Int, b::IntMap String, c::IntMap Number)
type CS = CompStorage SRow

uni0 :: CS
uni0=allocateStorageUniform u (Proxy2 :: Proxy2 IntMap)


--t = CompStorage $ set (unCompStorage v).b 4 "ffuu"

uni1 :: CS
uni1 = writeStorage uni0 13 { c:7.5, a:44 }
--uni1 = write uni0 (SProxy::SProxy "c") 13 7.5
uni2 :: CS
uni2 = writeStorage uni1 12 { b:"asdf" }
--uni2 = write uni1 (SProxy::SProxy "b") 12 "asdf"
uni3 :: CS
uni3 = writeStorage uni2 89 { a:45, b:"Frank"}
uni4 :: CS
uni4 = writeStorage uni3 12 {b:"KFJC", c:89.7}

fnABtoC :: Record (a::Int, b::String) -> Record (c::Number)
fnABtoC {a,b} = {c:val}
  where
    val = (toNumber (length b)) + (fa * 0.001)
    fa :: Number
    fa = toNumber a

fnBCtoB :: Record (b::String, c::Number) -> Record (b::String)
fnBCtoB {b,c} = {b:b <> (show c)}

uni5 :: CS
uni5 = mapFn uni4 fnABtoC

uni6 :: CS
uni6 = mapFn uni5 fnBCtoB

uni7 :: CS
uni7 = writeStorage uni6 45 {a:3, c:47.1}


rw = RProxy :: RProxy (c::Number, a::Int)

wr1 :: CS
wr1 = writeStorage uni3 99 { a : 17::Int, b : "written"}

rs = readStorage uni3 13 :: Record (c::Number, a::Int)
rwr1A = readStorage wr1 99 :: Record (a::Int)
rwr1B = readStorage wr1 99 :: Record (b::String)
rwr1Both = readStorage wr1 99 :: Record (a::Int, b::String)

het0 :: CS
het0 = allocateStorage (RProxy :: RProxy SRow)
het1 :: CS
het1 = writeStorage het0 55 { c:2.3 }
het2 = writeStorage het1 55 { c:5.6 }
--het1 = write het0 (SProxy::SProxy "c") 55 2.3

arrA :: Array (Tuple Int Int)
arrA = [ (Tuple 1 17), (Tuple 2 137), (Tuple 5 11)]
arrB :: Array (Tuple Int String)
arrB = [ (Tuple 1 "Enoch"), (Tuple 5 "Cassandra")]
arrC :: Array (Tuple Int Number)
arrC = [ (Tuple 2 44.1), (Tuple 5 16.3)]

cs :: CompStorage (a :: IntMap Int, b :: IntMap String, c :: IntMap Number)
cs = CompStorage { a: fromAssocArray arrA, b: fromAssocArray arrB, c: fromAssocArray arrC}
rpa = RProxy :: RProxy (c :: Number, b :: String)

fn1 :: Record (a :: Int, b :: String) -> Record (b :: String)
fn1 r = { b : (show r.a) <> r.b }


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow "Empty storage b::String"
  logShow uni0
  logShow "1-element assoc array for c::Number"
  logShow uni1
  logShow "Read c[13]:"
  logShow $ (readStorage uni1 13 :: Record (c::Number)).c
--  logShow $ read uni1 (SProxy::SProxy "c") 13
  logShow "Read c[13] after modifying b[12]"
  logShow $ (readStorage uni2 13 :: Record (c::Number)).c
--  logShow $ read uni2 (SProxy::SProxy "c") 13
  logShow "all indices that have (c::Number,b::String) attributes"
  logShow $ intersectIndices cs rpa
  logShow $ "append (show a[5]) and b[5]"
  logShow $ (applyFn cs fn1 5).b
  log $ "\n" <> "rs is readStorage uni3 13 for c and a"
  log $ show rs.a <> " " <> show rs.c
  log $ "\n" <> "rwr1A, rwr1B, rwr1Both"
  log $ (show rwr1A.a) <> " " <> (show rwr1B.b) <> " " <> (show rwr1Both.a) <> " " <> (show rwr1Both.b)
  log $ "\n" <> "uni4\n" <> show uni4
  log $ "\n" <> "uni5\n" <> show uni5
  log $ "\n" <> "uni6\n" <> show uni6
  log $ "\n" <> "uni7\n" <> show uni7

  log $ "\n" <> "het1\n" <> show het1

  log $ "\n" <> "het2\n" <> show het2

  log $ "\n" <> "minind(uni7)" <> show (minIndices uni7 (RProxy :: RProxy (a::Int, b::String)))
