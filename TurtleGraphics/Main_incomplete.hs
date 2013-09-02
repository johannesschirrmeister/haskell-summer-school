module Main where

import Data.Maybe
import Data.Angle
import SOE hiding (Angle)

type Distance = Int

data Command
  = Forward Int
  | Back Int
  | Right Int
  | Left Int
  | Penup
  | Pendown
  deriving (Show)

data TurtleState = TurtleState Point (Degrees Float) Bool

main::IO()
main = undefined

windowSize :: Size
windowSize = (600, 600)

initialState :: TurtleState
initialState = TurtleState (300, 300) 0 True

parseLogo :: String -> [Command]
parseLogo s = let getWords t = map words (lines t)
              in map (fromJust . parseWords) (getWords s) 

parseWords :: [String] -> Maybe Command
parseWords l = case l of
                ("forward":units:_) -> return (Forward (read units :: Int))
                ("back":units:_) -> return (Back (read units ::Int))
                ("right":units:_) -> return (Main.Right (read units ::Int))
                ("left":units:_) -> return (Main.Left (read units ::Int))
                ("penup":_) -> return Penup
                ("pendown":_) -> return Pendown
                _ -> fail "Could not identify command."

getLogo :: FilePath -> IO [Command]
getLogo f = do
              s <- readFile f
              let l = parseLogo s
              return l
                
calculatePosition :: Point -> Distance -> (Degrees Float) -> Point
calculatePosition (ox, oy) d a =   let
                                     x = (cosine theta) * (fromIntegral d) - (sine theta) * (fromIntegral ox)
                                     y = (sine theta) * (fromIntegral d) + (cosine theta) * (fromIntegral oy)
                                   in (round x, round y)
                                     where
                                       theta :: (Radians Float)   
                                       theta = radians a
                
processCommand :: Command -> TurtleState -> (Graphic, TurtleState)
processCommand c (TurtleState point angle drawing) = case c of
                Forward n ->  (line point calcPos, TurtleState calcPos angle drawing)
                                where calcPos = calculatePosition point n angle
                Back n -> (line point calcPos, TurtleState calcPos angle drawing)
                                where calcPos = calculatePosition point (-n) angle
                Main.Right n -> (line point point, TurtleState point (angle + (Degrees (fromIntegral n))) drawing)
                Main.Left n -> (line point point, TurtleState point (angle - (Degrees (fromIntegral n))) drawing)
                Penup -> (line point point, TurtleState point angle False)
                Pendown -> (line point point, TurtleState point angle False)
                _ ->  (line point point, TurtleState point angle drawing) 
              
processCommands :: [Command] -> TurtleState -> [Graphic]
processCommands [] _ = []
processCommands c ts = (fst pro) : (processCommands (tail c) (snd pro)) 
                        where pro = processCommand (head c) ts