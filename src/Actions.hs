-- | 'Actions' defines the functions which are called by the user.
module Actions where

import Char
import World
import List
import Monad

-- | The 'actions' function attempts to parse an input string into a 'Action' function.
actions :: String       -- ^ 'String' to be parsed as an 'Action'
		-> Maybe Action -- ^ Returns 'Just' 'Action'-function or 'Nothing' if not a valid 'Action'.
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions "open"    = Just open
actions _         = Nothing

-- | The 'commands' function attempts to parse an input string into a 'Command' function.
commands :: String        -- ^ 'String' to be parsed as a 'Command'
		 -> Maybe Command -- ^ Returns 'Just' 'Command', or 'Nothing' if not a valid 'Command'
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Nothing

-- | The 'directions' function attempts to parse an input string into a 'Direction'.
directions :: String 		  -- ^ 'String' to be parsed as a 'Direction'
		   -> Maybe Direction -- ^ Returns 'Just' 'Direction', or 'Nothing' if not a valid 'Direction'.
-- Convert the input string into lowercase
directions dir = case (map Char.toLower dir) of
		"north" -> Just North
		"n" -> Just North
		"south" -> Just South
		"s" -> Just South
		"west" -> Just West
		"w" -> Just West
		"east" -> Just East
		"e" -> Just East
		_ -> Nothing

{- 'objectHere' returns 'True' if an 'Object' with the given 'obj_name' exists appears in the given 'Room'. 
-}
objectHere :: String -> Room -> Bool
objectHere o rm = objectInArray o (objects rm)

{-| 'findObj' returns the 'Object' with a given 'obj_name' and a list of objects, return the object data. 
Requires the object to be in the list. 
-}
findObj :: String -> [Object] -> Object
findObj obj (d:ds)
	| obj == obj_name d = d
	| otherwise = findObj obj ds

-- | 'objectData' finds an 'Object' in a room description.
objectData :: String -> Room -> Object
objectData o rm = findObj o (objects rm)

{-| Given an 'Object' name and a 'Room' description, return a new 'Room' 
	description without that object 
-}
removeObject :: Object -> Room -> Room
removeObject o rm = rm { objects = list}
	where
		list = filter (\x -> o /= x) (objects rm)

{-| Given an 'Object' and a 'Room' description, return a new 'Room' description
   with that 'Object' added 
   -}
addObject :: Object -> Room -> Room
addObject o rm = rm { objects = o : objects rm }

{-| Given a game state and a 'Room' id, replace the old 'Room' information
	with new data. If a 'Room' with the given id does not already exist, it is added. 
 -}
updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata = gd { world = if (w == world gd) then newRoom : w else w }
	where
		newRoom = (rmid, rmdata)
		w = [if (fst r) == rmid then newRoom else r | r <- world gd]

{-| Given a game state and an 'obj_name', finds the 'Object' in the current
	'Room' and adds it to the player's 'inventory' 
   -}
addInv :: GameData -> Object -> GameData
addInv gd obj = gd { inventory = (obj : (inventory gd)) }

{-| Given a game state and an 'Object', remove the 'Object' from the
   'inventory'. 
   -}
removeInv :: GameData -> Object -> GameData
removeInv gd obj = gd { inventory = i }
	where
		i = filter (\x -> x /= obj) (inventory gd)

{-| Determines whether an 'Object' with a given 'obj_name' exists in
	the player's 'inventory' 
-}
carrying :: GameData -> String -> Bool
carrying gd obj = objectInArray obj (inventory gd)

-- | Determines whether a given 'Object' exists in the players' 'inventory'.
carryingObj :: GameData -> Object -> Bool
carryingObj gd obj = any (obj==) (inventory gd)

-- | Determines whether an 'Object' with a given 'obj_name' can be found in an array of 'Object's.
objectInArray :: String -> [Object] -> Bool
objectInArray obj xs = any (\x -> obj == obj_name x) xs

{-| 'move' is given a 'Direction' and a 'Room' to 'move' from 
	and returns the 'Room' id in that 'Direction', if it exists. 
-}
move :: Direction -> Room -> Maybe String
move dir rm = findExit dir (exits rm)
	where
		findExit :: Direction -> [Exit] -> Maybe String
		findExit _ [] = Nothing
		findExit dir (x:xs)
			| dir == exit_dir x = Just (room x)
			| otherwise = findExit dir xs

{-|	Given a string 'Direction' and a game state, update the game state with the new 'Location'. 
	If there is no 'Exit' that way, report an error. The 'String' is a message reported to the player.
-}
go :: Action
go dir state = case (directions dir) of
			Just d -> case move d (getRoomData state) of
				Just loc -> (state { location_id = loc }, "You go to the " ++ loc ++ ".")
				Nothing -> (state, "Cannot go there.")
			Nothing -> (state, "Go where?")

{-| 'get' removes an 'Object' from the current 'Room', and puts it in the player's 'inventory'.
	Only works if the 'Object' is in the current 'Room'. Uses 'objectHere'
	and 'removeObject' to remove the object, and 'updateRoom' to replace the
	'Room in the game state with the new 'Room' which doesn't contain the 'Object'.
-}
get :: Action
get obj state = do let loc = getRoomData state in
				  if objectHere obj loc then
					do	let obj' = objectData obj loc
						let state' = addInv state obj'
						let rmdata = removeObject obj' loc
						let rmid = location_id state
						let state'' = updateRoom state' rmid rmdata
						(state'', "You pick up " ++ obj ++ ".")
				else
					(state, obj ++ " is not here.")

{-| 'put' removes an 'Object' from the player's 'inventory', and puts it in the current 'Room'.
   Similar to 'get' but in reverse - find the 'Object' in the 'inventory', create
   a new 'Room' with the 'Object' in, update the game world with the new 'Room'.
-}
put :: Action
put obj state = if carrying state obj then
					do	let loc = getRoomData state
						let object = findObj obj (inventory state)
						let rmdata = addObject object loc
						let rmid = location_id state
						let nstate = removeInv state object
						(updateRoom nstate rmid rmdata, "You put down the " ++ obj ++ ".")
				else
					(state, obj ++ " is not in your inventory.")

{-| 'examine' returns a message giving the full description of the 'Object'
	with the given 'obj_name. If the 'Object' is not in the 'Room' or 
	the player's 'inventory', it reports an error message. 
-}
examine :: Action
examine obj state = (state, msg)
    where
        rmdata = getRoomData state
        obj' | objectHere obj rmdata = Just (objectData obj rmdata)
			 | carrying state obj 	 = Just (findObj obj (inventory state))
			 | otherwise 			 = Nothing
        msg = case obj' of
            Just obj'' -> obj_desc obj''
            Nothing -> "There's no such object here."

{-| 'pour' pours coffee into a mug. However, this is only done if the player is carrying
	both the pot and the mug, otherwise appropriate error messages are displayed.
-}
pour :: Action
pour obj state = if carryingObj state World.coffeepot then
                    if carryingObj state World.mug then
                        do
							let state' = removeInv state World.mug
							let state'' = addInv state' World.fullmug
							(state'', "You pour some coffee.")
                    else if carryingObj state World.fullmug then
						(state, "Your mug is already full!")
					else
						(state, "You don't have a mug to pour into!")
                 else
                     (state, "You don't have any coffee!")

{-| 'drink' makes the player drink the coffee (i.e. emptying the cup and updating
	the 'caffinated' flag. However, this is only done if the player has a full
	coffee mug!
-}
drink :: Action
drink obj state | carryingObj state World.fullmug =
					do	let state' = removeInv state World.fullmug
						let state'' = addInv state' World.mug
						(state'' { caffeinated = True }, "You drink the coffee.")
				| otherwise =
					(state, "You don't have a cup of coffee!")

{-| The 'open' function opens the door. However, this is only allowed if the 
	player has had coffee, otherwise it reports an error.
-}
open :: Action
open obj state 
	| caffeinated state
		= do
			let objs = objects World.hall
			let room = Room World.openedhall World.openedexits objs
			(updateRoom state "hall" room, "You open the door.")
	| otherwise
		= (state, "It's too heavy!")

{-| 'inv' lists what the player is carrying. -}
inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything."
         showInv xs = "You are carrying:\n" ++ showInv' xs
		 --showInv' xs = foldr (/x -> (show x) ++ "\n" ++) "" xs
         showInv' [x] = show x
         showInv' (x:xs) = show x ++ "\n" ++ showInv' xs

{-| The 'quit' function quits the game, saying "Bye bye."-}
quit :: Command
quit state = (state { finished = True }, "Bye bye.")

