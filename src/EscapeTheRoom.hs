{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module EscapeTheRoom where

-- | Made by @unaimillan
import CodeWorld
import Data.Text (Text)
import EscapeTheRoom.Levels


--data Tile = Wall | Floor | Button Color | Door Color | Exit
-- data DoorColor = Red | Blue | Green
-- data ButtonColor = Red | Blue | Green
-- data Coords = Coords Integer Integer
data Dir = Dir Integer Integer
-- data Level = Level Coords (Coords -> Tile)
-- | State is player coords and colors of opened doors/pressed buttons
data State = State Coords [DoorColor]


-- | My level definition
--myLevel :: Level
--myLevel = Level (Coords 0 0) levelMap [] -- (openDoors [red, blue] levelMap)
myLevel = level10

levelMap :: Coords -> Tile
levelMap (Coords i j)
  | abs i > 9 || abs j > 9 = Wall  -- walls around map
  | trigger 0 7  = Exit
  | trigger 0 0  = Floor
  | trigger 0 1 = Door CPink
  | trigger (-6) 0 = Button CPink
  | trigger (-2) 0 = Door CBlue
  | trigger (-4) 0 = Door CBlue
  | trigger 0 (-2) = Door CGreen
  | trigger 2 0 = Door CRed
  | trigger (-1) 0 = Button CRed
  | trigger 0 (-1) = Button CBlue
  | trigger (-3) 0 = Button CBlue
  | trigger (1) 0 = Button CGreen
  | trigger (-6) (-6) = Door CRed
  | trigger (6) (-6) = Door CBlue
  | i == j       = Wall
  | -i == j      = Wall
  | i < 0 && j == 1 = Wall
  | -8 <= i && i < 0 && j == -1 = Wall
  | j == 6 && (-6) < i && i < 6 = Door CYellow
  | trigger (-4) 5 = Button CYellow
  | otherwise = Floor -- floor otherwise
  where
    trigger x y = (i == x) && (j == y)


-- | Helper functions
drawPlayerAt :: Coords -> Picture
drawPlayerAt (Coords i j) = 
  translated (fromIntegral i) (fromIntegral j) (lettering "🚶")

floorTile :: Picture
floorTile = colored yellow (solidRectangle 0.95 0.95)

wallTile :: Picture
wallTile = colored black (solidRectangle 0.9 0.9)

cmpDColors :: DoorColor -> DoorColor -> Bool
cmpDColors a b = colorMap a == colorMap b

colorMap :: DoorColor -> Color
colorMap CRed = red
colorMap CGreen = green
colorMap CBlue = blue
colorMap CPink = pink
colorMap CPurple = purple
colorMap CYellow = yellow
colorMap CCyan = cyan
colorMap COrange = orange
colorMap CWhite = white
colorMap CGray = grey
colorMap CBrown = brown
colorMap CBlack = black
colorMap CDarkRed = dark red
colorMap CDarkGreen = dark green
colorMap CLightBlue = light blue

pictureOfButton :: DoorColor -> Picture
pictureOfButton c = colored (colorMap c) (solidCircle 0.3)

buttonTile :: DoorColor -> Picture
buttonTile c = pictureOfButton c <> floorTile

doorTile :: DoorColor -> Picture
doorTile c = pictureOfButton c <> wallTile

exitTile :: Picture
exitTile = scaled (1/5) (1/5) (target 5)
  where
    target :: Integer -> Picture
    target 0 = blank
    target n = target (n - 1) <> colored c (solidRectangle d d)
      where
        d = 0.95 * fromIntegral n
        c | even n     = red
          | otherwise = black

drawTile :: Tile -> Picture
drawTile Wall = wallTile
drawTile Exit = exitTile
drawTile (Button clr) = buttonTile clr
drawTile (Door clr) = doorTile clr
drawTile Floor = floorTile


-- | All the assignments are together
-- | Assignment 2.1.1 -- Validate moves
tryMove :: Coords -> [DoorColor] -> Bool
tryMove coords colors
  | canMove ((properLevelMap colors) coords) = True
  | otherwise = False

canMove :: Tile -> Bool
canMove Floor = True
canMove (Button _) = True
canMove Exit = True
canMove _ = False


-- | Assignment 2.1.2 -- 21x21 level with mapping function
-- | R.I.P.
drawLevelMap :: (Coords -> Tile) -> Picture
drawLevelMap func = pictures (map (drawColumn (container func)) [-10..10])

drawColumn :: (Int -> Int -> Picture) -> Int -> Picture
drawColumn wrap x = pictures (map (wrap x) [-10..10])

container :: (Coords -> Tile) -> Int -> Int -> Picture
container func i j = translated x y (drawTile (func (Coords i j)))
  where
    x = fromIntegral i
    y = fromIntegral j
  
pictureOfLevel :: [DoorColor] -> Picture
pictureOfLevel colors = drawLevelMap (properLevelMap colors)


-- | Assignment 2.1.3 -- open doors
openDoors :: [DoorColor] -> (Coords -> Tile) -> (Coords -> Tile)
openDoors colors func = result
  where
    result coords
      | isMatchingColor ans colors = Floor -- first check is obligatory
      | otherwise = ans
        where
          ans = func coords

oneOf :: DoorColor -> [DoorColor] -> Bool
oneOf clr = any (cmpDColors clr)

isMatchingColor :: Tile -> [DoorColor] -> Bool
isMatchingColor (Door cls) colors = oneOf cls colors
isMatchingColor _ _ = False

properLevelMap :: [DoorColor] -> (Coords -> Tile)
properLevelMap opened = openDoors opened mymap
  where
    (Level _ mymap _) = myLevel


-- | Assignment 2.1.4
genNewCoords :: Text -> State -> Coords
genNewCoords dirStr old_state
    | tryMove new_coords old_colors = new_coords
    | otherwise                     = old_coords
  where
    new_coords = sumCoords old_coords (convCoords dirStr)
    State old_coords old_colors = old_state
    
-- | Convert coordinates from string to d[Coords]
convCoords :: Text -> Coords
convCoords "Up" = Coords 0 1
convCoords "Down" = Coords 0 (-1)
convCoords "Left" = Coords (-1) 0
convCoords "Right" = Coords 1 0
convCoords _ = Coords 0 0

sumCoords :: Coords -> Coords -> Coords
sumCoords first second = Coords (a+d) (b+e)
  where
    Coords a b = first
    Coords d e = second
    
toggleColors :: Coords -> [DoorColor] -> [DoorColor]
toggleColors coords old_colors
  | is_button && one_of_colors = filter (not . cmpDColors cur_color) old_colors
  | is_button && not one_of_colors = cur_color:old_colors
  | otherwise  = old_colors
    where
      one_of_colors = oneOf cur_color old_colors
      is_button = isButtonTile current
      current = levelFunc coords
      --current = ((properLevelMap old_colors) coords)
      (Button cur_color) = current
      (Level _ levelFunc _) = myLevel

isButtonTile :: Tile -> Bool
isButtonTile (Button _) = True
isButtonTile _ = False


-- | interactionOf part
initialWorld :: State
initialWorld = State initCoords []
  where
    (Level initCoords _ _) = myLevel

updateWorld :: Double -> State -> State
updateWorld _ = id

-- genNewCoords String Coords -> Coords
-- toggleColors [DoorColor] newCoords -> [DoorColor]
handleWorld :: Event -> State -> State
handleWorld (KeyPress dir) state = State new_coords new_colors
  where
    State _ old_colors = state
    new_coords = genNewCoords dir state
    new_colors = toggleColors new_coords old_colors
handleWorld _ state = state

drawWorld :: State -> Picture
drawWorld (State coords colors) = drawPlayerAt coords <> pictureOfLevel colors

solution4 :: IO ()
solution4 = interactionOf initialWorld updateWorld handleWorld drawWorld


-- | Assignment 3.2.1
-- | The type of an 'interactionOf' function.
type InteractionOf world = 
  world
  -> (Double -> world -> world)
  -> (Event -> world -> world)
  -> (world -> Picture)
  -> IO ()

-- | Make 'interactionOf' resettable on Esc.
withReset :: InteractionOf world
  -> world
  -> (Double -> world -> world)
  -> (Event -> world -> world)
  -> (world -> Picture)
  -> IO ()
withReset func initial update handle draw 
  = func initial update new_handle draw
  where
    new_handle (KeyPress "Esc") _ = initial
    new_handle event state = handle event state

-- interactionOf :: InteractionOf world
newInteractionOf :: InteractionOf world
newInteractionOf = (withReset interactionOf)

solution5 :: IO ()
solution5 = newInteractionOf initialWorld updateWorld handleWorld drawWorld


-- | Assignment 3.2.2
-- | Interaction state for 'world' with start screen.
data WithStartScreen world
  = StartScreen -- ^ Start screen.
  | GameOn world -- ^ Game is on with 'world' state.

startScreen :: Picture
startScreen = title <> subtitle <> heart <> background
  where
    bigF = 2
    smallF = 1
    sc x = scaled x x
    title = sc bigF (lettering "Escape the Room")
    subtitle = translated 0 (-2) (sc smallF (lettering "Press Space to start"))
    heart = translated 0 (-5) (sc 3 (lettering "❤️"))
    background = colored (light pink) (solidRectangle 50 50)

neoInit :: (world) -> WithStartScreen world
neoInit _ = StartScreen -- | (GameOn world)

neoUpdate :: (Double -> world -> world) 
  -> Double -> WithStartScreen world -> WithStartScreen world
neoUpdate _ _ = id -- define behavior for animation

neoHandle :: world -> (Event -> world -> world) 
  -> Event -> WithStartScreen world -> WithStartScreen world
neoHandle initial _ (KeyPress " ") StartScreen = GameOn initial
neoHandle _ _ _ StartScreen = StartScreen
neoHandle _ func event (GameOn state) = GameOn (func event state)

neoDraw :: (world -> Picture) 
  -> WithStartScreen world -> Picture
neoDraw _ StartScreen = startScreen
neoDraw func (GameOn state) = func state

-- | Add start screen to 'interactionOf'.
withStartScreen
  :: InteractionOf (WithStartScreen world)
  -> InteractionOf world
withStartScreen func initial update handle draw
  = func myInitial myUpdate myHandle myDraw
  where
    myInitial = neoInit initial
    myUpdate = neoUpdate update
    myHandle = neoHandle initial handle
    myDraw = neoDraw draw

solution6 :: IO ()
solution6 = (withStartScreen (withReset interactionOf)) 
  initialWorld updateWorld handleWorld drawWorld


solution7 :: IO ()
solution7 = drawingOf( (colored (translucent red) (solidCircle 1)) 
  <> solidCircle 0.5)


run :: IO ()
run = solution6
