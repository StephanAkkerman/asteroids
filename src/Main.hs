module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display
import Data.List
import System.Random
import System.IO.Unsafe

-- GamerField consists of the world
data GamerField = PlayingField [Asteroid] Player [Lazer] [UFO] Level
                | Paused [Asteroid] Player [Lazer] [UFO] Level
                | SaveScore Score
                | PlayerDeath1 [Asteroid] Player [Lazer] [UFO] Age
                | PlayerDeath2 [Asteroid] Player [Lazer] [UFO] Age
                | LevelSelect
                | GameOver Score

-- Here we define the data types we will be using for our game
data Asteroid = Asteroid Location Angle Speed Size   
data Player   = Player   Location Angle Speed Score
data Lazer    = Lazer    Location Angle Speed Age 
              | UFOLazer Location Angle Speed Age
data UFO      = UFO      Location Angle Speed Age

type Location  = (Float, Float)
type Angle     = Float
type Size      = Float
type Speed     = Float
type Score     = Int
type Age       = Int
type Level     = Int

levelOne :: GamerField
levelOne = PlayingField 
              [
              Asteroid (200, 250) (randomFloat 20 80)(randomFloat 1 3) (randomFloat 0 270),
              Asteroid ((-100), (-300)) (randomFloat 20 80.001) (randomFloat 1 3.001) (randomFloat 0 270.001),
              Asteroid (150, (-250)) (randomFloat 20 80.001) (randomFloat 1 3.002) (randomFloat 0 270.002)
              ]
              (Player (0,0) 90 0 0)
              []
              [
              UFO ((-300), 300) 225 (randomFloat 1 2) 0
              ]
              1

levelTwo :: Score -> GamerField
levelTwo score = PlayingField
                [
                    Asteroid (150, 370) (randomFloat 20 80.03) (randomFloat 1 3.03) (randomFloat 0 270.03),
                    Asteroid ((-370), (-300)) (randomFloat 20 80.01) (randomFloat 1 3.01) (randomFloat 0 270.01),
                    Asteroid (150, (-250)) (randomFloat 20 80.02) (randomFloat 1 3.02) (randomFloat 0 270.02),
                    Asteroid (90, (-100)) (randomFloat 21 80.04) (randomFloat 1 3.04) (randomFloat 0 270.04)
                ]
                (Player (0,0) 90 0 score)
                []
                [
                    UFO ((-300), 300) 225 (randomFloat 1 2) 0
                ]
                2

levelThree :: Score -> GamerField
levelThree score = PlayingField
                [
                    Asteroid (150, 370) (randomFloat 20 80.03)(randomFloat 1 3.03) (randomFloat 0 270.03),
                    Asteroid ((-370), (-300)) (randomFloat 20 80.01) (randomFloat 1 3.01) (randomFloat 0 270.01),
                    Asteroid (150, (-250)) (randomFloat 20 80.02) (randomFloat 1 3.02) (randomFloat 0 270.02)
                ]
                (Player (0,0) 90 0 score)
                []
                [
                    UFO ((-300), 300) 225 (randomFloat 1 2) 0,
                    UFO ((-300), 150) 225 (randomFloat 1.01 2.01) 0
                ]
                3

-- Function to generate random Float between a range
randomFloat :: Float -> Float -> Float
randomFloat min max = unsafePerformIO (getStdRandom (randomR (min, max)))
                                  
-- Function for putting the GamerField into a Picture to display
createPicture :: GamerField -> Picture
createPicture (PlayingField asteroids (Player (x, y) ap sp sc) lazers ufs _)
    = pictures [ast, ship, lightrays, ufolightrays, ufos, score]
    where
        ast          = color asteroidColor (pictures [translate u v (polygon (asteroidShape aSize)) | Asteroid (u, v) aSize _ _ <- asteroids])
        ship         = color shipColor (pictures [translate x y (rotate ap (polygon (playerShape 10)))])
        lightrays    = color red (pictures [translate u v (circle 3) | Lazer (u,v) _ _ _ <- lazers])
        ufolightrays = color yellow (pictures [translate u v (circle 3) | UFOLazer (u,v) _ _ _ <- lazers])
        ufos         = color aquamarine (pictures [translate u v (circleSolid 20) | UFO (u,v) _ _ _ <- ufs])
        score        = color white (pictures [translate (-350) (350) (scale 0.15 0.15 (text ("Score: " ++ (show sc))))])

createPicture (PlayerDeath1 asteroids (Player (x, y) ap sp sc) lazers ufs age)
    = pictures [ast, ship, lightrays, ufolightrays, ufos, score]
    where
        ast          = color asteroidColor (pictures [translate u v (polygon (asteroidShape aSize)) | Asteroid (u, v) aSize _ _ <- asteroids])
        ship         = color black (pictures [translate x y (rotate ap (polygon (playerShape 10)))])
        lightrays    = color red (pictures [translate u v (circle 3) | Lazer (u,v) _ _ _ <- lazers])
        ufolightrays = color yellow (pictures [translate u v (circle 3) | UFOLazer (u,v) _ _ _ <- lazers])
        ufos         = color aquamarine (pictures [translate u v (circleSolid 20) | UFO (u,v) _ _ _ <- ufs])
        score        = color white (pictures [translate (-350) (350) (scale 0.15 0.15 (text ("Score: " ++ (show sc))))])

createPicture (PlayerDeath2 asteroids (Player (x, y) ap sp sc) lazers ufs age)
    = pictures [ast, ship, lightrays, ufolightrays, ufos, score]
    where
        ast          = color asteroidColor (pictures [translate u v (polygon (asteroidShape aSize)) | Asteroid (u, v) aSize _ _ <- asteroids])
        ship         = color shipColor (pictures [translate x y (rotate ap (polygon (playerShape 10)))])
        lightrays    = color red (pictures [translate u v (circle 3) | Lazer (u,v) _ _ _ <- lazers])
        ufolightrays = color yellow (pictures [translate u v (circle 3) | UFOLazer (u,v) _ _ _ <- lazers])
        ufos         = color aquamarine (pictures [translate u v (circleSolid 20) | UFO (u,v) _ _ _ <- ufs])
        score        = color white (pictures [translate (-350) (350) (scale 0.15 0.15 (text ("Score: " ++ (show sc))))])

createPicture (GameOver score) = pictures [scale 0.3 0.3 . translate (-400) 0 
                                    . color red . text $ "Game Over!",
                                    scale 0.1 0.1 . translate (-1150) (-550)
                                    . color white . text $ 
                                    "Your score was: " ++ show score,
                                    scale 0.1 0.1 . translate (-2000) (-1000)
                                    . color white . text $ 
                                    "Press the space-bar to restart game, F1 to save score, or 'L' to select a level"]

createPicture LevelSelect = pictures [scale 0.3 0.3 . translate (-400) 400
                                    . color red . text $ "Choose a level!",
                                    scale 0.1 0.1 . translate (-1150) (-550)
                                    . color white . text $ 
                                    "Level 1 ",
                                    scale 0.1 0.1 . translate (-1150) (-750)
                                    . color white . text $ 
                                    "Level 2",
                                    scale 0.1 0.1 . translate (-1150) (-950)
                                    . color white . text $ 
                                    "Level 3"]

createPicture (Paused _ _ _ _ _) = pictures [scale 0.3 0.3 . translate (-900) 0 
                                    . color red . text $ "Game is currently paused!",
                                    scale 0.1 0.1 . translate (-950) (-550)
                                    . color white . text $ 
                                    "Press 'p' to continue"]

createPicture (SaveScore score) = pictures [scoresText, saveScreen, s1, s2, s3, s4, s5]
                                    where 
                                        scoresText = pictures [scale 0.35 0.35 . translate (-200) (800) . color aquamarine . text $ "High Scores"]
                                        saveScreen = pictures [scale 0.1 0.1 . translate (-750) (-550) . color white . text $ "Press F1 to save or F2 to cancel"]
                                        s1 = pictures [scale 0.25 0.25 . translate (-200) (0) . color red . text $ "5. " ++ show (readScore !! 4)]
                                        s2 = pictures [scale 0.25 0.25 . translate (-200) (200) . color red . text $ "4. " ++ show (readScore !! 3)]
                                        s3 = pictures [scale 0.25 0.25 . translate (-200) (400) . color red . text $ "3. " ++ show (readScore !! 2)]
                                        s4 = pictures [scale 0.25 0.25 . translate (-200) (600) . color red . text $ "2. " ++ show (readScore !! 1)]
                                        s5 = pictures [scale 0.25 0.25 . translate (-200) (800) . color red . text $ "1. " ++ show (readScore !! 0)]

-- Read and write score
svScore contents = unsafePerformIO (appendFile "score.txt" (" " ++ (show contents) ++ " "))
readScore = reverse . sort . map readInt . words . unsafePerformIO . readFile $ "score.txt"

readInt :: String -> Int
readInt str = read str

-- 
-- Object shapes
--
playerShape :: Size -> [Point]
playerShape size = [(size, -size), (size, size), (2 * (-size), 0)]

asteroidShape :: Size -> [Point]
asteroidShape size = [(0, size), (0.75 * size, 0.75 * size), (size, 0), (0.75 * size, (-0.75) * size), (0, -size), ((-0.75) * size, (-0.75) * size), (-size, 0), ((-0.75) * size, 0.75 * size)] -- [(size * (-0.1), size), (size * 0.5, size * 0.6), (size, size * 0.1), (size * 0.9, size * (-0.2)), (size * 0.45, size * (-0.9)), (size * 0.1, size * (-0.7)), (size * (-0.15), size * (-0.8)), (size * (-0.1), size * (-0.8)), (size * (-0.6), size * (-0.6)), (size * (-0.9), size * 0.8)]

--
-- Object colors
--
shipColor :: Color
shipColor = chartreuse -- Eventueel random maken door een list van Color te maken en dan random Int

asteroidColor :: Color
asteroidColor = greyN 0.5

-- | Handle user input
inputHandler :: Event -> GamerField -> GamerField
-- Left and Right buttons change angle
-- Up and Down change movement speed
-- Spacebar shoots lazers
inputHandler (EventKey key Down _ _) (PlayingField asteroids (Player (x, y) ap sp sc) lazers ufos lvl)
             | key == SpecialKey KeyUp    || key == Char 'w' = PlayingField asteroids (Player (x, y) ap 2.5 sc) lazers ufos lvl
             | key == SpecialKey KeyDown  || key == Char 's' = PlayingField asteroids (Player (x, y) ap 0 sc) lazers ufos lvl
             | key == SpecialKey KeyRight || key == Char 'd' = PlayingField asteroids (Player (x, y) (rotateRight ap) sp sc) lazers ufos lvl
             | key == SpecialKey KeyLeft  || key == Char 'a' = PlayingField asteroids (Player (x, y) (rotateLeft ap) sp sc) lazers ufos lvl
             | key == SpecialKey KeySpace                    = PlayingField asteroids (Player (x, y) ap sp sc) ((newLazer (x,y) ap):lazers) ufos lvl
             | key == Char 'p'                               = Paused asteroids (Player (x, y) ap sp sc) lazers ufos lvl
             | otherwise                                     = PlayingField asteroids (Player (x, y) ap sp sc) lazers ufos lvl
             where rotateRight angle | angle == 360 = rotateRight 0
                                     | otherwise    = angle + 22.5
                   rotateLeft angle  | angle == 0   = rotateLeft 360
                                     | otherwise    = angle - 22.5
                   newLazer loc angle               = Lazer loc angle 50 0

-- Press spacebar if the game is over to restart
inputHandler (EventKey key Down _ _) (GameOver score)
             | key == SpecialKey KeySpace = levelOne
             | key == Char 'l'            = LevelSelect
             | key == SpecialKey KeyF1    = SaveScore score

-- Press F1 to save the game, F2 to return to previous screen
inputHandler (EventKey key Down _ _) (SaveScore score)
             | key == SpecialKey KeyF1 = (svScore score) `seq` (SaveScore score)
             | key == SpecialKey KeyF2 = GameOver score

-- Press number to select level
inputHandler (EventKey key Down _ _) LevelSelect
             | key == Char '1' = levelOne
             | key == Char '2' = levelTwo 0
             | key == Char '3' = levelThree 0

-- Press 'p' if the game is paused to continue
inputHandler (EventKey (Char 'p') Down _ _) (Paused asteroids (Player (x, y) ap sp sc) lazers ufos lvl) = PlayingField asteroids (Player (x, y) ap sp sc) lazers ufos lvl

-- In case none of the specified keys are pressed, change nothing
inputHandler _ g = g

-- Function to update GamerField
step :: Float -> GamerField -> GamerField
step _ (GameOver score) = GameOver score
step _ LevelSelect      = LevelSelect
step _ (PlayerDeath1 asteroids p@(Player _ _ _ sc) lazers ufos age) | age `mod` 5 == 0 = PlayerDeath2 asteroids p lazers ufos (age + 1)
                                                                    | otherwise        = PlayerDeath1 asteroids p lazers ufos (age + 1)
step _ (PlayerDeath2 asteroids p@(Player _ _ _ sc) lazers ufos age) | age `mod` 5 == 0 = PlayerDeath1 asteroids p lazers ufos (age + 1)
                                                                    | age > 120        = GameOver sc
                                                                    | otherwise        = PlayerDeath2 asteroids p lazers ufos (age + 1)
step _ (SaveScore score) = SaveScore score
step _ (Paused a b c d e) = Paused a b c d e
step time (PlayingField asteroids p@(Player loc ap sp sc) lazers ufos lvl)
    | playerDied p   = PlayerDeath1 asteroids p lazers ufos sc
    | completedLevel = nextLevel
    | otherwise      = PlayingField (concat (map asteroidUpdate asteroids)) (Player (newPosition loc ap sp) ap sp (updateScore sc)) ((concat (map lazerUpdate lazers))++(concat (map newUFOLazers ufos))) (concat (map ufoUpdate ufos)) lvl
    where
        -- Updates the lazer's position and age
        lazerUpdate :: Lazer -> [Lazer]
        lazerUpdate (Lazer lLoc angle speed age) | age < 7 && not (any (collides lLoc) asteroids) && not (any (collidesUFO lLoc) ufos ) = [Lazer (newPosition lLoc angle speed) angle speed (age + 1)]
                                                 | age < 7 && any (collides lLoc) asteroids                                             = []
                                                 | age < 7 && any (collidesUFO lLoc) ufos                                               = []
                                                 | otherwise                                                                            = []
        lazerUpdate (UFOLazer lLoc angle speed age) | age < 7 && not (any (collides lLoc) asteroids) = [UFOLazer (newPosition lLoc angle speed) angle speed (age + 1)]
                                                    | age < 7 && any (collides lLoc) asteroids       = []
                                                    | otherwise                                      = []

        -- Check if the player died
        playerDied :: Player -> Bool
        playerDied p = any (collides loc) asteroids || any (collidesUFO loc) ufos || any (== True) [collidesPlayer ll p |(UFOLazer ll _ _ _) <- lazers]

        -- Update the score
        updateScore :: Score -> Score
        updateScore score | any (== True) [collides ll a | (Lazer ll _ _ _ ) <- lazers, a <- asteroids] = score + 1
                          | any (== True) [collidesUFO ll u | (Lazer ll _ _ _ ) <- lazers, u <- ufos]   = score + 3
                          | otherwise                                                                   = score
        
        -- Update an asteroid
        asteroidUpdate :: Asteroid -> [Asteroid]
        asteroidUpdate a@(Asteroid loc size speed angle) | any (== True) [collides ll a |(Lazer ll _ _ _) <- lazers] = breakAsteroid a
                                                         | otherwise                                                 = [Asteroid (newPosition loc angle speed) size speed angle]

        -- Break an asteroid
        breakAsteroid :: Asteroid -> [Asteroid]
        breakAsteroid (Asteroid loc size speed aAngle) | (size / 2) > 10 = [Asteroid (newPosition (newRandomLoc loc 0 10) randomAngle speed) (size / 2) speed randomAngle,
                                                                            Asteroid (newPosition (newRandomLoc loc (-10) 0) (180 - randomAngle) speed) (size / 2) speed (180 -randomAngle)]
                                                       | otherwise       = []
                                                        where
                                                             randomAngle = randomFloat (aAngle + 90) (aAngle + 180)

        -- Update a UFO
        ufoUpdate :: UFO -> [UFO]
        ufoUpdate u@(UFO uloc angle speed age) | any (== True) [collidesUFO ll u | (Lazer ll _ _ _ ) <- lazers, u <- ufos] = []
                                               | any (collides uloc) asteroids                                             = []
                                               | age `mod` 59 == 0                                                         = [UFO (newPosition uloc angle speed) (getAngle uloc loc) speed (age + 1)]
                                               | otherwise                                                                 = [UFO (newPosition uloc angle speed) angle speed (age + 1)]

        -- Create a new UFO lazer every 2 seconds                                                  
        newUFOLazers :: UFO -> [Lazer]
        newUFOLazers u@(UFO loc angle speed age) | age `mod` 120 /= 0 = []
                                                 | otherwise          = [UFOLazer (newPosition loc angle speed) angle (speed + 50) 0]

        -- Check if the level has been completed
        completedLevel :: Bool
        completedLevel | null asteroids && null ufos = True
                       | otherwise                   = False

        nextLevel :: GamerField
        nextLevel | lvl == 1  = levelTwo sc
                  | lvl == 2  = levelThree sc
                  | otherwise = levelThree sc


-- Result is in radians so we need to convert that to degrees
getAngle :: Location -> Location -> Angle
getAngle (x,y) (i,j) = 180 - newAngle * 57.296
        where y1 = j - y
              x1 = i - x
              newAngle = atan2 y1 x1

-- Calculates a new location based on old location and a range
newRandomLoc :: Location -> Float -> Float -> Location
newRandomLoc (x,y) min max = ((randomFloat min max) + x, (randomFloat min max) + y)

-- Calculates the new position of an object, based on the location, angle and speed
newPosition :: Location -> Angle -> Speed -> Location
newPosition loc ang sp = ((fst vector) * sp + (fst loc) + outOfBoundsMove (fst loc), (snd vector) * sp + (snd loc) + outOfBoundsMove (snd loc))
           where
                vector = calcHeadingVector ang 
            
-- Functions for checking if an object collides with an asteroid/UFO/player
collides :: Location -> Asteroid -> Bool
collides (x, y) (Asteroid (aX, aY) size _ _) = sqrt (deltaX ** 2 + deltaY ** 2) < size
                                 where
                                      deltaX = aX - x
                                      deltaY = aY - y

collidesUFO :: Location -> UFO -> Bool
collidesUFO (x, y) (UFO (aX, aY) _ _ _) = sqrt (deltaX ** 2 + deltaY ** 2) < 20
                                 where
                                      deltaX = aX - x
                                      deltaY = aY - y

collidesPlayer :: Location -> Player -> Bool
collidesPlayer (x, y) (Player (aX, aY) _ _ _) = sqrt (deltaX ** 2 + deltaY ** 2) < 20
                                 where
                                      deltaX = aX - x
                                      deltaY = aY - y

-- Given an angle, returns the heading vector of the form (Vector x, Vector y)
calcHeadingVector :: Angle -> (Float, Float)
calcHeadingVector ang = (cos angleRadians, sin angleRadians)
    where 
         angleRadians = pi - ang * pi / 180

-- This makes sure we can cross the borders of the game
outOfBoundsMove :: Float -> Float
outOfBoundsMove loc | loc < -400 = 800
                    | loc > 400  = -800
                    | otherwise  = 0

-- Main function that launches the game --
main = play
         (InWindow "Asteroids" (800,800) (20,20)) -- Display mode
         black                                    -- Background color
         60                                       -- Frames per second (FPS)
         LevelSelect                              -- The initial world
         createPicture                            -- An action to convert the world to a picture
         inputHandler                             -- A function to handle input events
         step                                     -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.
