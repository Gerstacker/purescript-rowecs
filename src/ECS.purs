module ECS where

import Data.Maybe
import Data.Record (insert)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), SProxy(SProxy), RProxy)
import Type.Proxy (Proxy2(Proxy2))
import Type.Row (Cons, Nil, kind RowList)
import Data.IntMap as IM

class Storage a (m :: Type -> Type) where
  allocate :: m a
  get :: m a -> Int -> Maybe a
  set :: m a -> Int -> a -> m a


instance storageIntMap :: Storage a IM.IntMap where
  allocate = IM.empty
  get im ind = IM.lookup ind im
  set im ind val = IM.insert ind val im



type ECS rowst = (Record (storage :: (Record rowst), nextID :: Int))


class AllocateStorage (xs :: RowList) (row :: # Type) a (row' :: # Type) (m :: Type -> Type)
    | xs -> row row' a
  where
    allocateStorageImpl :: RLProxy xs -> Proxy2 m -> ECS row'


instance allocateStorageNil :: AllocateStorage Nil row a () m where
  allocateStorageImpl _ _ = { storage : {}, nextID : 0}

instance allocateStorageCons ::
  ( IsSymbol name
  , RowCons name a trash row
  , AllocateStorage tail row b tailRow' m
  , RowLacks name tailRow'
  , RowCons name (m a) tailRow' row'
  , Storage a m
  ) => AllocateStorage (Cons name a tail) row a row' m where
  allocateStorageImpl _ _ =
    (rest { storage = stor})
    where
      stor = insert nameP val rest.storage
      nameP = SProxy :: SProxy name
      val = allocate
      rest = allocateStorageImpl (RLProxy :: RLProxy tail) (Proxy2 :: Proxy2 m)


allocateStorage :: forall m row xs a row'
  . RowToList row xs
  => AllocateStorage xs row a row' m
  => Storage a m
  => RProxy row
  -> Proxy2 m
  -> ECS row'
allocateStorage _ = allocateStorageImpl (RLProxy :: RLProxy xs)
