# Proposed Fix for Hittable List Instance

The current implementation of `Hittable [obj]` has a bug where the search for the closest object doesn't correctly update the maximum distance (`tmax`) after a hit is found in the first element.

## Correct Implementation

Using a fold is more idiomatic and ensures the `closest` distance is always respected:

```haskell
instance Hittable obj => Hittable [obj] where
    hit ray tmin tmax = foldl' findClosest Nothing
      where
        findClosest acc obj =
            let currentTMax = maybe tmax (\info -> info.hitT) acc
            in case hit ray tmin currentTMax obj of
                Just info -> Just info
                Nothing   -> acc
```

Alternatively, fixing the recursive `go` function:

```haskell
instance (Hittable obj) => Hittable [obj] where
    hit _ _ _ [] = Nothing
    hit ray tmin tmax (o : os) = go tmax initInfo os
      where
        initInfo = hit ray tmin tmax o
        go _ acc [] = acc
        go closest acc (obj : objs) =
            let currentClosest = maybe closest (\info -> info.hitT) acc
            in case hit ray tmin currentClosest obj of
                Just hitInfo -> go hitInfo.hitT (Just hitInfo) objs
                Nothing -> go currentClosest acc objs
```
