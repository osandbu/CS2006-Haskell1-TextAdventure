{-| 'SaveLoad' contains functions that are only relevant to saving and loading. These were originally in
Actions.hs, but it was felt that there was enough disparity in the intent of the code for it to warrant its
own module -}
module Saveload where

import IO
import World
import Actions

{-| Error handler to catch save IO exceptions. The game should still be able
to be played, even if the saved game file can't be written. -}
saveHandler :: IOError -> IO ()
saveHandler e
	| isAlreadyInUseError e = putStrLn "ERROR: Could not save game, file is is use"
	| otherwise = putStrLn "ERROR: Something unexpected happened! Game not saved"

{-| Error handler to catch load IO exceptions. The game should still be able
to be played, even if the saved game file can't be saved. -}
loadHandler :: GameData -> IOError -> IO GameData
loadHandler state e
	| isDoesNotExistError e = do	putStrLn "NO SAVED GAME FILE FOUND"
					return state
	| otherwise = do	putStrLn "ERROR: Something unexpected happened! Game not loaded"
				return state

{-| The 'findRoom' function takes two arguments: a room id and a list of tuples of room names and rooms and returns the corresponding 'Room' description. Assumes that a room with the given room ID exists in the list of tuples. -}
findRoom :: String -> [(String, Room)] -> Room
findRoom rm (wd:wds)
	| fst wd == rm 	= snd wd
	| otherwise	= findRoom rm wds

{-| Serialise the 'Object's in a gameworld (list of 'Room's). -}
showRooms :: [(String, Room)] -> String
showRooms [] = ""
showRooms ((rmid,room):xs) =  "\n" ++ rmid ++ "\n" ++ show [obj_name o | o <- (objects room)] ++ showRooms xs

{-| Unserialise a list of alternating room names and a list of objects names in that room. -}
readRooms :: [String] -> [(String, [String])]
readRooms []   = []
readRooms list = ((list !! 0, read (list !! 1)):readRooms (drop 2 list))

{-| Build a list of 'Object's for each room from a list of object names -}
buildRooms :: [(String, [String])] -> [(String, [Object])]
buildRooms []     = []
buildRooms (x:xs) = ((fst x, [findObj o gameObjects | o <- snd x]):buildRooms xs)

{- Given a list of tuples with 'Room' ids and [Object] arrays, create a list of 'Room' ids 
and rooms, representing the game world. -}
buildGameWorld :: [(String, [Object])] -> [(String, Room)]
buildGameWorld list = [(fst r, (findRoom (fst r) gameworld) { objects = snd r }) | r <- list]

{-| Build a 'GameData' state from a list of 'String's. -}
buildGameData :: [String] -> GameData
buildGameData gdlist = GameData location_id world inventory' poured' caffeinated' finished'
	where
		location_id = gdlist !! 0
		inventory 	= gdlist !! 1
		inventory' = [findObj o gameObjects | o <- read inventory]
		poured 	= gdlist !! 2
		poured' = read poured :: Bool
		caffeinated	= gdlist !! 3
		caffeinated' = read caffeinated :: Bool
		finished = gdlist !! 4
		finished' = read finished :: Bool
		rooms = readRooms (drop 5 gdlist)
		rooms' = buildRooms rooms
		world = buildGameWorld rooms'
			

{-| The 'save' function saves the current game. -}
save :: GameData -> String -> IO () 
save state filename = do	writeFile filename statestring
				putStrLn "GAME SAVED"
	where statestring = 		location_id state
			++ "\n" ++ 	show [obj_name o | o <- (inventory state)]
			++ "\n" ++ 	show (poured state)
			++ "\n" ++ 	show (caffeinated state)
			++ "\n" ++ 	show (finished state)
			++		showRooms (world state)


{-| The 'load' function loads a previously saved game. -}
load :: String -> IO GameData
load filename = do 
					gdstring <- readFile' filename
					let gdlist 	= lines gdstring
					let gd = buildGameData gdlist
					putStrLn "GAME LOADED"
					return gd

{- Strict version of readFile, from: http://users.aber.ac.uk/afc/stricthaskell.html
This was needed because recursing repl in Adventure.hs after reading a file would
leave the file open and locked, so it couldn't be written to. This meant that a load
then save would fail. -}

-- Begin stolen code
hGetContents' hdl = do	e <- hIsEOF hdl 
			if e then return []
				else do	c <- hGetChar hdl
					cs <- hGetContents' hdl
					return (c:cs)


readFile' fn = do	hdl <- openFile fn ReadMode 
			xs <- hGetContents' hdl
			hClose hdl
			return xs 
-- End stolen code