module Main where

import World
import Actions
import Saveload

import IO
import Monad
import System.Exit
import Text.Regex

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."
			 
			
{-| The 'processCommands' function takes a game state and a 'String'
	of commands as arguments, splits the given string around " then " 
	and processes one command at the time.
-}			
processCommands :: GameData -> String -> (GameData, String)
processCommands state cmds = processCommands' state (split " then " cmds)
	where
		processCommands' :: GameData -> [String] -> (GameData, String)
		processCommands' state [cmd] = process state (words cmd)
		processCommands' state (cmd:cmds) = do
			let state' = process state (words cmd)
			--print (snd state')
			processCommands' (fst state') cmds
		split :: String -> String -> [String]
		split delim s = splitRegex (mkRegex delim) s
		
{-| Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}
process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case actions cmd of
                            Just fn -> fn arg state
                            Nothing -> (state, "I don't understand")
process state [cmd]     = case commands cmd of
                            Just fn -> fn state
                            Nothing -> (state, "I don't understand")
process state _ = (state, "I don't understand")

{-| 'repl' is the game loop, which takes in commands from the user and
	processes them. -}
repl :: GameData -> IO GameData
repl state | finished state = return state
repl state = do print state
                putStr "What now? "
                hFlush stdout
                cmd <- getLine
                case cmd of  -- injected checking for save/load here, as this is the last place we can call IO from
                    "save" -> do catch (save state "game.sav") saveHandler
                                 repl state -- need a recursive call to keep the game going
                    "load" -> do state' <- catch (load "game.sav") (loadHandler state)
                                 repl state'
                    _      -> do let (state', msg) = processCommands state cmd
                                 putStrLn msg
                                 if (won state') then do putStrLn winmessage
                                                         return state'
                                                 else repl state'	

{-| main method required for the -}
main :: IO ()
main = do repl initState
          return ()
