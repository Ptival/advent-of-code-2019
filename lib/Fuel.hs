-- |

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fuel (
  Mass (..),
  fuelForMass,
  fuelForMassAndFuel,
  ) where

newtype Fuel = Fuel { fuel :: Int }
  deriving ( Eq, Num, Show )

newtype Mass = Mass { mass :: Int }
  deriving ( Eq, Num, Show )

fuelAsMass :: Fuel -> Mass
fuelAsMass Fuel{..} = Mass fuel

fuelForMass :: Mass -> Fuel
fuelForMass Mass{..} = Fuel $ max 0 ((mass `div` 3) - 2)

fuelForMassAndFuel :: Mass -> Fuel
fuelForMassAndFuel m =
  let fuelForThisMass = fuelForMass m in
  let fuelForItsFuel  =
        if fuelForThisMass == Fuel 0
        then Fuel 0
        else fuelForMassAndFuel (fuelAsMass fuelForThisMass) in
  fuelForThisMass + fuelForItsFuel
