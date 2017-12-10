module ECS where

import Data.Maybe
import Data.Record (insert, get) as R
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), SProxy(SProxy), RProxy)
import Type.Proxy (Proxy2(Proxy2))
import Type.Row (Cons, Nil, kind RowList)
import Data.IntMap as IM

class Storage (m :: Type -> Type) a where
  allocate :: m a
  get :: m a -> Int -> Maybe a
  set :: m a -> Int -> a -> m a


instance storageIntMap :: Storage IM.IntMap a where
  allocate = IM.empty
  get im ind = IM.lookup ind im
  set im ind val = IM.insert ind val im



type ECS rowst (m :: Type -> Type) = (Record (storage :: (Record rowst), nextID :: Int))
--unECS (ECS r) = r


read :: forall rowst name a m tail
  . Storage m a
  => IsSymbol name
  => RowCons name (m a) tail rowst
  => ECS rowst m -> SProxy name -> Int -> Maybe a
read ecs spr ind = get v ind
  where
    v = (R.get spr ecs.storage) :: m a




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
      rest = allocateStorageImpl (RLProxy :: RLProxy tail) (Proxy2 :: Proxy2 m)


allocateStorage :: forall m row xs a row'
  . RowToList row xs
  => AllocateStorage xs row a row' m
  => Storage m a
  => RProxy row
  -> Proxy2 m
  -> ECS row' m
allocateStorage _ = allocateStorageImpl (RLProxy :: RLProxy xs)
