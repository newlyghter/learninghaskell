module SelfRef where

data Room = Room String [(String, Room)]

describe :: Room -> String
describe (Room s _) = s

move :: Room -> String -> Maybe Room
move (Room _ directions) direction = lookup direction directions

world :: Room
world = meadow
  where
    meadow = Room "It's a flowery meadow next to a cliff." [("Stay",meadow),("Enter cave",cave)]
    cave = Room "You are in a cave" [("Exit",meadow),("Go deeper",tunnel)]
    tunnel = Room "This is a very dark tunnel. It seems you can either go left or right."
                  [("Go back",cave),("Go left",pit),("Go right",treasure)]
    pit = Room "You fall into a pit. There is no way out." []
    treasure = Room "A green light from a terminal fills the room. The terminal says <<loop>>."
                    [("Go back",tunnel)]

play :: Room -> [String] -> [String]
play room [] = [describe room]
play room (d:ds) = case move room d of Nothing -> [describe room]
                                       Just r -> describe room : play r ds