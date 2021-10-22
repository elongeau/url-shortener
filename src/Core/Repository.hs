module Core.Repository where

data Repository m k v = Repository
  { save :: v -> m v,
    findById :: k -> m (Maybe v)
  }
