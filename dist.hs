import qualified Data.Map                      as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB =
    Map.fromList [("Arkham", (42.4, -70.7)), ("Innsmouth", (52.62, -45.45))]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
LatLongToRads (lat,long) = (rlat,rlong)
 where
  rlat = toRadians lat
  rlong = toRadians long


