-- | 'World' describes the different objects in the game world and their types.
module World where

import Char

-- | 'Action's require an argument and tend to update the game state (examine does not)
type Action  = String -> GameData -> (GameData, String)

-- | 'Command's do not require any arguments
type Command = GameData -> (GameData, String)

-- | 'Object's are objects in the game world, which can be picked up using the 'get' function in 'Actions', and dropped using 'put' from 'Actions'
data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String }
   deriving (Eq, Read)

-- | Data constructor for 'Exit's.
data Exit = Exit { exit_dir :: Direction, -- ^ The 'Direction' in which a player has to go to leave a 'Room' through this 'Exit'
                   exit_desc :: String,	  -- ^ A description of where this 'Exit' leads
                   room :: String		  -- ^ A single word identifier of the 'Room' the exit leads to.
				   }
   deriving Eq

-- | Data constructor for 'Room's.
data Room = Room { room_desc :: String, -- ^ A description of the 'Room'
                   exits :: [Exit], 	-- ^ The 'Exit's leading from this 'Room'
                   objects :: [Object]	-- ^ The 'Object's contained in this 'Room'
				   }
   deriving Eq

-- | Data constructor for 'GameData', i.e. the game state.
data GameData = GameData { location_id :: String,	  -- ^ the 'Room' identifier of the location where the player currently is.
                           world :: [(String, Room)], -- ^ Tuple containing 'Room' identifiers and 'Room' descriptions.
                           inventory :: [Object],	  -- ^ 'Object's the player is 'carrying'.
                           poured :: Bool,			  -- ^ Whether coffee has been poured.
                           caffeinated :: Bool,		  -- ^ Whether coffee has been drunk.
                           finished :: Bool			  -- ^ Set to True at the end.
                         }

-- | Data constructors for 'Direction's.
data Direction = North | West | South | East
	deriving (Eq, Show)

-- | Returns true if the player has won.
won :: GameData -> Bool
won gd = location_id gd == "street"

instance Show Object where
   show obj = obj_longname obj

instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs
        
instance Show GameData where
    show gd = show (getRoomData gd)

-- | mug, fullmug and coffeepot are all 'Object's in the gameworld.
mug, fullmug, coffeepot :: Object
mug       = Obj "mug" "a coffee mug" "A coffee mug"
fullmug   = Obj "fullmug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"

{- All the objects in the game (required for save/load) -}
gameObjects = [mug, fullmug, coffeepot]

bedroom, kitchen, hall, street :: Room

bedroom = Room "You are in your bedroom."
               [Exit North "To the north is a kitchen. " "kitchen"]
               [mug]

kitchen = Room "You are in the kitchen."
               [Exit South "To the south is your bedroom. " "bedroom",
                Exit West "To the west is a hallway. " "hall"]
               [coffeepot]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit East "To the east is a kitchen. " "kitchen"]
            []

-- New data about the hall for when we open the door

openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit East "To the east is a kitchen. " "kitchen",
               Exit West "To the west is the front door. " "street"]

street = Room "You have made it out of the house."
              [Exit East "You can go back inside if you like. " "hall"]
              []

gameworld = [("bedroom", bedroom),
             ("kitchen", kitchen),
             ("hall", hall),
             ("street", street)]

{-| The 'initState' function initializes the starting state of the game. -}
initState :: GameData
initState = GameData "bedroom" gameworld [] False False False

{-| The 'getRoomData' function returns the room the player is currently in. -}
getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))
