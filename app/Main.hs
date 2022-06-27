module Main where

import Prelude 
import UI.NCurses
import Control.Concurrent (threadDelay)
import Control.Monad.State.Lazy (liftIO)
import Control.Monad.Extra (allM)
import System.Directory.Internal (andM)
import Data.List ( intersect )
import Data.List.Unique ( allUnique )

type XYpos = (Integer, Integer)
data Player = Player
  { p  :: [XYpos],
  d :: Direction
  } deriving (Eq, Show)

data Direction = Direction 
 { posdif :: XYpos,
   symbol :: String
 } deriving (Eq, Show)


main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  (x,y) <-updateWindow w $ do
    clear
    moveCursor 0 0
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    windowSize 
  render
  let players = setupPlayers [[(5, 5)], [(5, y-5)]]
  playing w players

-- spelloop
playing :: Window -> [Player] -> Curses ()
playing w players = loop where
  loop = do
    noCollision <- andM (allM (isNotWallCollision w) players) (isNotPlayerCollision players)
    -- om spelet inte är över
    if noCollision then do
      nPlayers <- getKeyPress players w
      nPlayers <- updateWindow w $ do
        p <- setPlayers nPlayers
        mapM_ updatePlayer p
        moveCursor 0 0
        return p
      render
      playing w nPlayers
    -- om spelet är över
    else do
      updateWindow w $ do
        (x,y) <- windowSize
        moveCursor (div x 2) (div y 2)
        drawString "GAME OVER"
      render
      waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

-- använd kanppen q för att lämna spelet efter dess slut
waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
  loop = do
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop
      Just ev' -> if p ev' then return () else loop

--Skapar spelarna i deras startpositioner
setupPlayers :: [[XYpos]] -> [Player]
setupPlayers = map (\x -> Player {p = x, d = (Direction {posdif = (1,0), symbol = "v"})})

-- flyttar spelarna i dens satta riktingnen
setPlayers :: [Player] -> Update [Player]
setPlayers pls = return (map setPlayer pls)

setPlayer :: Player -> Player
setPlayer (Player pos dir) = Player { p = addDirPos (head pos) dir : pos, d = dir }

--Ansätter ny riktningen
newDir :: Player -> Direction -> Player
newDir pl dir = pl {d = dir }

--Hämtar knapptryck wasd flyttar spelare 1 och arrowkeys spelare 2, annat kanpptryck returnerar samma riktningar
getKeyPress ::  [Player] -> Window -> Curses [Player]
getKeyPress [p1,p2] w = do
  liftIO $ threadDelay 100000
  ev <- getEvent w (Just 1)
  case ev of
    
    Nothing                              -> return [p1,p2]
    Just (EventCharacter 'w')            -> return [newDir p1 (Direction {posdif = (-1,0), symbol = "^" }), p2]
    Just (EventCharacter 's')            -> return [newDir p1 (Direction {posdif = (1,0), symbol = "v" }), p2]
    Just (EventCharacter 'a')            -> return [newDir p1 (Direction {posdif = (0,-1), symbol = "<" }), p2]
    Just (EventCharacter 'd')            -> return [newDir p1 (Direction {posdif = (0,1), symbol = ">" }), p2]

    Just (EventSpecialKey KeyUpArrow)    -> return [p1, newDir p2 (Direction {posdif = (-1,0), symbol = "^" })]
    Just (EventSpecialKey KeyDownArrow)  -> return [p1, newDir p2 (Direction {posdif = (1,0), symbol = "v" })]
    Just (EventSpecialKey KeyLeftArrow)  -> return [p1, newDir p2 (Direction {posdif = (0,-1), symbol = "<" })]
    Just (EventSpecialKey KeyRightArrow) -> return [p1, newDir p2 (Direction {posdif = (0,1), symbol = ">" })]

    Just (EventCharacter _)              -> return [p1,p2]


-- Kollar om spelarnas senaste förfluyttning fått dom kollidera
isNotPlayerCollision :: [Player] -> Curses Bool
isNotPlayerCollision [Player (h1:t1) _,Player (h2:t2) _] = do
  if not (allUnique (h1 : (h2:t2))) || not (allUnique (h2 : (h1:t1))) then return False
  else return True

-- Kollar om spelarens senaste förflyttning fått den att kollidera med en vägg
isNotWallCollision :: Window -> Player -> Curses Bool
isNotWallCollision w (Player ((x,y):t) (Direction (xd,yd) symbol)) = enclosed w (x+xd) (y+yd)

--Lägger till riktningens symbolen på den rutan som spelaren förflyttas till
addDirPos :: XYpos -> Direction -> XYpos
addDirPos (x,y) (Direction (xd,yd) symbol) = (x+xd,y+yd)

-- Ritar upp det funktionerna gjort
updatePlayer :: Player -> Update ()
updatePlayer (Player pos (Direction (xd,yd) symbol)) = do
  uncurry moveCursor (head pos)
  drawString symbol
  uncurry moveCursor (pos!!1)
  drawString "#"
  
 


  