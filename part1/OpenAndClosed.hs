module OpenAndClosed where

-- datatype version is closed and isn't extensible
-- set of cases is fixed, handling them all in one place

{-
data Vehicle = Car String | Airplane String

sound :: Vehicle -> String
sound (Car _) = "brum brum"
sound (Airplane _) = "zooooom"
-}

-- class-based solution is open and extensible
-- we can add new cases, even in other modules

{-
data Car = Car String
data Airplane = Airplane String

class VehicleClass a where
  sound :: a -> String

instance VehicleClass Car where
  sound (Car _) = "brum brum"

instance VehicleClass Airplane where
  sound (Airplane _) = "zooooom"
-}