-- |

{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad           ( forM_ )
import Data.List.Split         ( splitOn )
import Data.String.Interpolate
import Prelude                 hiding ( Left, Right )

data Direction
  = Down
  | Left
  | Right
  | Up
  deriving ( Show )

data Input = Input
  { direction :: Direction
  , distance  :: Int
  }
  deriving ( Show )

readDirection :: Char -> Direction
readDirection 'D' = Down
readDirection 'L' = Left
readDirection 'R' = Right
readDirection 'U' = Up
readDirection d   = error [i|Could not read direction #{d}|]

readInput :: String -> Input
readInput (dir : dis) = Input { direction = readDirection dir
                              , distance  = read dis
                              }
readInput [] = error "readInput: empty input"

data Position = Position { x :: Int
                         , y :: Int
                         }

instance Show Position where
  show Position{..} = [i|(#{x}, #{y})|]

data Path = Path
  { currentPosition :: Position
  , path            :: [Position]
  }
  deriving ( Show )

move ::
  Position -> -- ^ start point
  Direction ->
  Int ->        -- ^ distance
  Position
move Position{..} Down  d = Position { x
                                     , y = y - d
                                     }
move Position{..} Left  d = Position { x = x - d
                                     , y
                                     }
move Position{..} Right d = Position { x = x + d
                                     , y
                                     }
move Position{..} Up    d = Position { x
                                     , y = y + d
                                     }

origin :: Position
origin = Position { x = 0
                  , y = 0
                  }

-- | Compute all coordinates encountered after the origin, assuming paths start
-- at (0, 0)
pathFromInputs :: [Input] -> Path
pathFromInputs = foldl foldInput Path { currentPosition = origin
                                      , path            = []
                                      }
  where
    foldInput :: Path -> Input -> Path
    foldInput Path{..} Input{..} =
      let newPosition = move currentPosition direction distance in
      Path { currentPosition = newPosition
           , path            = newPosition : path
           }

main :: IO ()
main =
  do let inputs = map  (map readInput . splitOn ",") . lines $ input
     forM_ inputs $ print . pathFromInputs

-- Part 1

input :: String
input = [i|R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83|]
