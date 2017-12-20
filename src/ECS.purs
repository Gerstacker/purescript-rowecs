module ECS where

import Data.Maybe

import Control.Monad.Rec.Class (tailRec)
import Data.IntMap as IM
import Data.Record (insert, get, set) as R
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), SProxy(SProxy), RProxy)
import Type.Proxy (Proxy2(Proxy2))
import Type.Row (Cons, Nil, kind RowList)

class Storage (m :: Type -> Type) a where
  allocate :: m a
  get :: m a -> Int -> Maybe a
  set :: m a -> Int -> a -> m a


instance storageIntMap :: Storage IM.IntMap a where
  allocate = IM.empty
  get im ind = IM.lookup ind im
  set im ind val = IM.insert ind val im



type ECS rowst (m :: Type -> Type) = (Record (storage :: (Record rowst), nextID :: Int))


read :: forall rowst name a m tail
  . Storage m a
  => IsSymbol name
  => RowCons name (m a) tail rowst
  => ECS rowst m -> SProxy name -> Int -> Maybe a
read ecs spr ind = get v ind
  where
    v = (R.get spr ecs.storage) :: m a

write :: forall rowst name a m tail
  . Storage m a
  => IsSymbol name
  => RowCons name (m a) tail rowst
  => ECS rowst m -> SProxy name -> Int -> a -> ECS rowst m
write ecs spr ind val = ecs { storage = stor' }
  where
    intmap = (R.get spr ecs.storage) :: m a
    intmap' = set intmap ind val
    stor' = R.set spr intmap' ecs.storage



class AllocateStorage (xs :: RowList) (row :: # Type) a (row' :: # Type) (m :: Type -> Type)
    | xs m -> row row' a, row' -> m row
  where
    allocateStorageImpl :: RLProxy xs -> Proxy2 m -> ECS row' m

instance allocateStorageNil :: AllocateStorage Nil row a () m where
  allocateStorageImpl _ _ = { storage : {}, nextID : 0}

instance allocateStorageCons ::
  ( IsSymbol name
  , RowCons name a trash row
  , AllocateStorage tail row b tailRow' m
  , RowLacks name tailRow'
  , RowCons name (m a) tailRow' row'
  , Storage m a
  ) => AllocateStorage (Cons name a tail) row a row' m where
  allocateStorageImpl _ _ =
    (rest { storage = stor})
    where
      stor = R.insert nameP val rest.storage
      nameP = SProxy :: SProxy name
      val = allocate
      rest = allocateStorageImpl (RLProxy :: RLProxy tail) (Proxy2 :: Proxy2 m) :: ECS tailRow' m

allocateStorage :: forall m row xs a row'
  . RowToList row xs
  => AllocateStorage xs row a row' m
  => Storage m a
  => RProxy row
  -> Proxy2 m
  -> ECS row' m
allocateStorage _ = allocateStorageImpl (RLProxy :: RLProxy xs)

{-

class ReadStorage (srow :: # Type) (xs :: RowList) (row :: # Type) a (row' :: # Type) (m :: Type -> Type)
    | xs m -> row row' a, row' -> m row
  where
    readStorageImpl :: RLProxy xs -> ECS row' m -> Int -> Record row

instance readStorageNil :: ReadStorage srow Nil row a () m where
  readStorageImpl _ _ _= {}

instance readStorageCons ::
  ( IsSymbol name
  , RowCons name a trash row
  , ReadStorage srow tail row b tailRow' m
  , RowLacks name tailRow'
  , RowCons name (m a) tailRow' row'
  , Storage m a
  ) => ReadStorage srow (Cons name a tail) row a row' m where
  readStorageImpl _ _ =
    (rest { storage = stor})
    where
      stor = R.insert nameP val rest.storage
      nameP = SProxy :: SProxy name
\      val = read
      rest = readStorageImpl (RLProxy :: RLProxy tail) (Proxy2 :: Proxy2 m) :: ECS tailRow' m


readStorage :: forall m srow row xs a row'
  . RowToList row xs
  => ReadStorage srow xs row a row' m
  => Storage m a
  => ECS srow
  -> Int
  -> Record row
readStorage = readStorageImpl (RLProxy :: RLProxy xs)
-}
