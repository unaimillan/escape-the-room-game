{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module EscapeTheRoom where

-- | Made by @unaimillan
import CodeWorld
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import EscapeTheRoom.Levels

--data Tile = Wall | Floor | Button Color | Door Color | Exit
-- data DoorColor = Red | Blue | Green
-- data ButtonColor = Red | Blue | Green
-- data Coords = Coords Integer Integer
data Dir =
  Dir Integer Integer

-- data Level = Level Coords (Coords -> Tile)
-- | State is player coords and colors of opened doors/pressed buttons
data State =
  State Coords (Coords -> Tile) [DoorColor]

-- | Type for `levelMap` function
type LevelMap = Coords -> Tile

-- | My level definition
--myLevel :: Level
--myLevel = Level (Coords 0 0) levelMap [] -- (openDoors [red, blue] levelMap)
-- myLevel :: Level
-- myLevel = level13
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
exitTile = scaled (1 / 5) (1 / 5) (target 5)
  where
    target :: Integer -> Picture
    target 0 = blank
    target n = target (n - 1) <> colored c (solidRectangle d d)
      where
        d = 0.95 * fromIntegral n
        c
          | even n = red
          | otherwise = black

drawTile :: Tile -> Picture
drawTile Wall = wallTile
drawTile Exit = exitTile
drawTile (Button clr) = buttonTile clr
drawTile (Door clr) = doorTile clr
drawTile Floor = floorTile

-- | All the assignments are together
-- | Assignment 2.1.1 -- Validate moves
tryMove :: Coords -> State -> Bool
tryMove coords state
  | canMove (properLevelMap state coords) = True
  | otherwise = False

canMove :: Tile -> Bool
canMove Floor = True
canMove (Button _) = True
canMove Exit = True
canMove _ = False

-- | Assignment 2.1.2 -- 21x21 level with mapping function
-- | R.I.P.
drawLevelMap :: (Coords -> Tile) -> Picture
drawLevelMap func = pictures (map (drawColumn (container func)) [-10 .. 10])

drawColumn :: (Int -> Int -> Picture) -> Int -> Picture
drawColumn wrap x = pictures (map (wrap x) [-10 .. 10])

container :: (Coords -> Tile) -> Int -> Int -> Picture
container func i j = translated x y (drawTile (func (Coords i j)))
  where
    x = fromIntegral i
    y = fromIntegral j

pictureOfLevel :: State -> Picture
pictureOfLevel curSt = drawLevelMap (properLevelMap curSt)

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

properLevelMap :: State -> (Coords -> Tile)
properLevelMap (State _ func opened) = openDoors opened func

-- | Assignment 2.1.4
genNewCoords :: Text -> State -> Coords
genNewCoords dirStr old_state
  | tryMove new_coords old_state = new_coords
  | otherwise = old_coords
  where
    new_coords = sumCoords old_coords (convCoords dirStr)
    State old_coords _ old_colors = old_state

-- | Convert coordinates from string to d[Coords]
convCoords :: Text -> Coords
convCoords "Up" = Coords 0 1
convCoords "Down" = Coords 0 (-1)
convCoords "Left" = Coords (-1) 0
convCoords "Right" = Coords 1 0
convCoords _ = Coords 0 0

sumCoords :: Coords -> Coords -> Coords
sumCoords first second = Coords (a + d) (b + e)
  where
    Coords a b = first
    Coords d e = second

toggleColors :: Coords -> State -> [DoorColor]
toggleColors coords (State _ levelFunc old_colors)
  | is_button && one_of_colors = filter (not . cmpDColors cur_color) old_colors
  | is_button && not one_of_colors = cur_color : old_colors
  | otherwise = old_colors
  where
    one_of_colors = oneOf cur_color old_colors
    is_button = isButtonTile current
    current = levelFunc coords
      --current = ((properLevelMap old_colors) coords)
    (Button cur_color) = current

isButtonTile :: Tile -> Bool
isButtonTile (Button _) = True
isButtonTile _ = False

-- | interactionOf part
initialWorld :: State
initialWorld = initLevelMap level13

updateWorld :: Double -> State -> State
updateWorld _ = id

-- genNewCoords String Coords -> Coords
-- toggleColors [DoorColor] newCoords -> [DoorColor]
handleWorld :: Event -> State -> State
handleWorld (KeyPress dir) state = State new_coords curLevelMap new_colors
  where
    State _ curLevelMap old_colors = state
    new_coords = genNewCoords dir state
    new_colors = toggleColors new_coords state
handleWorld _ state = state

drawWorld :: State -> Picture
drawWorld curSt = drawPlayerAt coords <> pictureOfLevel curSt
  where
    State coords _ _ = curSt

solution4 :: IO ()
solution4 = interactionOf initialWorld updateWorld handleWorld drawWorld

-- | Assignment 3.2.1
-- | The type of an 'interactionOf' function.
type InteractionOf world
   = world -> (Double -> world -> world) -> (Event -> world -> world) -> (world -> Picture) -> IO ()

-- | Make 'interactionOf' resettable on Esc.
withReset ::
     InteractionOf world
  -> world
  -> (Double -> world -> world)
  -> (Event -> world -> world)
  -> (world -> Picture)
  -> IO ()
withReset func initial update handle = func initial update new_handle
  where
    new_handle (KeyPress "Esc") _ = initial
    new_handle event state = handle event state

-- interactionOf :: InteractionOf world
interactionWithReset :: InteractionOf world
interactionWithReset = withReset interactionOf

solution5 :: IO ()
solution5 = interactionWithReset initialWorld updateWorld handleWorld drawWorld

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

neoInit :: world -> WithStartScreen world
neoInit _ = StartScreen -- | (GameOn world)

neoUpdate ::
     (Double -> world -> world)
  -> Double
  -> WithStartScreen world
  -> WithStartScreen world
neoUpdate _ _ = id -- define behavior for animation

neoHandle ::
     world
  -> (Event -> world -> world)
  -> (Event -> WithStartScreen world -> WithStartScreen world)
neoHandle initial _ (KeyPress " ") StartScreen = GameOn initial
neoHandle _ _ _ StartScreen = StartScreen
neoHandle _ func event (GameOn state) = GameOn (func event state)

neoDraw :: (world -> Picture) -> WithStartScreen world -> Picture
neoDraw _ StartScreen = startScreen
neoDraw func (GameOn state) = func state

-- | Add start screen to 'interactionOf'.
withStartScreen :: InteractionOf (WithStartScreen world) -> InteractionOf world
withStartScreen func initial update handle draw =
  func myInitial myUpdate myHandle myDraw
  where
    myInitial = neoInit initial
    myUpdate = neoUpdate update
    myHandle = neoHandle initial handle
    myDraw = neoDraw draw

interactionWithStartScreen :: InteractionOf world
interactionWithStartScreen = withStartScreen interactionWithReset

solution6 :: IO ()
solution6 =
  interactionWithStartScreen initialWorld updateWorld handleWorld drawWorld

solution7 :: IO ()
solution7 =
  drawingOf (colored (translucent red) (solidCircle 1) <> solidCircle 0.5)

-- Assignment 4.1.2
-- | Special datatype for several levels
data WithLevel level world =
  WithLevel level world

winnerLevel :: Level
winnerLevel = Level (Coords 0 0) lvlMap []
  where
    n = 10
    lvlMap (Coords i j)
      | i == 0 && j == 0 = Button CRed
      | abs i == abs j && abs i > 2 && abs j > 2 = Floor
      | abs i == 2 || abs j == 2 = Exit
      | abs i == 1 || abs j == 1 = Door CGreen
      | abs i > n - 1 || abs j > n - 1 = Wall
      | otherwise = Wall

-- High order approach
-- | Initialize game 'State' for a given 'LevelMap'.
initLevelMap :: Level -> State
initLevelMap (Level coords func colors) = State coords func colors

-- | Is current level complete given some game 'State'?
isLevelComplete :: State -> Bool
isLevelComplete (State coords func _) = isFinal
  where
    isFinal =
      case func coords of
        Exit -> True
        _ -> False

-- | Returns first level of the list
nextLevelMap :: [Level] -> Maybe Level
nextLevelMap [] = Nothing
nextLevelMap (l:ls) = Just l

-- | Turn an interactive program into one with multiple levels.
withManyLevels ::
     [Level] -- ^ A list of levels.
  -> (Level -> State) -- ^ Initialize world for level.
  -> (State -> Bool) -- ^ Is this level complete?
  -> InteractionOf (WithLevel [Level] State) -- ^ 'interactionOf'.
  -> InteractionOf State
withManyLevels lvls initLevel isFinished func initial update handle draw =
  func
    (multiInit lvls)
    multiUpdate
    (multiHandle initLevel isFinished handle)
    multiDraw
  where
    multiUpdate _ = id
    multiDraw (WithLevel lvl st) = draw st

multiInit :: [Level] -> WithLevel [Level] State
multiInit lvls = WithLevel (drop 1 lvls) (initLevelMap first)
  where
    first = fromMaybe winnerLevel (nextLevelMap lvls)

-- multiUpdate - dummy
multiHandle ::
     (Level -> State)
  -> (State -> Bool)
  -> (Event -> State -> State)
  -> (Event -> WithLevel [Level] State -> WithLevel [Level] State)
multiHandle initFunc doneFunc handle event (WithLevel lvls st)
  | not (doneFunc st) = WithLevel lvls (handle event st)
  | otherwise = WithLevel (drop 1 lvls) new_st
  where
    new_lvl = fromMaybe winnerLevel (nextLevelMap lvls)
    new_st = initFunc new_lvl

-- multiDraw -- draw initial if there are 13 levels, draw final if 0 of them
interactionWithMany :: InteractionOf State
interactionWithMany =
  withManyLevels
    levels
    initLevelMap
    isLevelComplete
    interactionWithStartScreen
  --interactionOf

solution8 :: IO ()
solution8 = interactionWithMany initialWorld updateWorld handleWorld drawWorld

run :: IO ()
run = solution8
