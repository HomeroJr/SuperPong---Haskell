module Main where
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data GameAttribute = Score Int

type PongObject = GameObject ()
data GameState = Level Int | GameInit Int



width = 700
height = 700
w = 700.0
h = 700.0

type PongAction a = IOGame GameAttribute () GameState TileAttribute a
data TileAttribute = NoTileAttribute
type PongTile = Tile TileAttribute

bmpList :: FilePictureList
bmpList = [("tex.mbp",Nothing)]


tileSize :: GLdouble
tileSize = width/10

t::PongTile
t = (0, False, 0.0, NoTileAttribute)

m::PongTile
m = (2, True, 0.0, NoTileAttribute)


main :: IO ()
main = do
        let winConfig = ((0,0),(500,500),"SuperPong!")
            bmpList = [("tex.bmp", Nothing)]
            gameMap = textureMap 0 250 250 250.0 250.0
            bar = objectGroup "barGroup" [createBar]
            ball = objectGroup "ballGroup" [createBall, createBall2, createBall3]
            initScore = Score 0
            input = [(Char 'a', StillDown, moveLeft),
                     (Char 'd', StillDown, moveRight),
                     (Char 'e', Press, \_ _ -> funExit)
                    ]

        funInit winConfig gameMap [bar,ball] (GameInit 0) initScore input gameCycle (Timer 40) bmpList


--restartGame :: Modifiers -> Position -> PongAction()
--restartGame _ _ = do
--  lvl <- getGameState
--  setGameState (GameInit 0)

 
createBall :: PongObject
createBall = let ballPic = Basic (Circle 8.0 1.0 0.0 0.0 Filled)
             in object "ball" ballPic False (125,125) (-5,5) ()


createBall2 :: PongObject
createBall2 = let ballPic = Basic (Circle 8.0 1.0 1.0 0.0 Filled)
             in object "ball2" ballPic False (125,125) (-10,10) ()


createBall3 :: PongObject
createBall3 = let ballPic = Basic (Circle 8.0 0.0 1.0 1.0 Filled)
             in object "ball3" ballPic False (125,125) (-13,13) ()


createBar :: PongObject
createBar = let barBound = [(-25,-6),(25,-6),(25,6),(-25,6)]
                barPic = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
            in object "bar" barPic False (125,30) (0,0) ()


moveRight :: Modifiers -> Position -> PongAction()
moveRight _ _ = do
 obj <- findObject "bar" "barGroup"
 (pX,pY) <- getObjectPosition obj
 (sX,_)  <- getObjectSize obj
 if (pX + (sX/2) + 15 <= 250)
   then (setObjectPosition ((pX + 15),pY) obj)
   else (setObjectPosition ((250 - (sX/2)),pY) obj)


moveLeft :: Modifiers -> Position -> PongAction()
moveLeft _ _ = do
 obj <- findObject "bar" "barGroup"
 (pX,pY) <- getObjectPosition obj
 (sX,_)  <- getObjectSize obj
 if (pX - (sX/2) - 15 >= 0)
   then (setObjectPosition ((pX - 15),pY) obj)
   else (setObjectPosition  (sX/2,pY) obj)


gameCycle :: PongAction ()
gameCycle = do
 (Score n) <- getGameAttribute
 state <- getGameState
 printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0
 case state of
   (GameInit levelInit) -> do
    setGameState (Level levelInit)
   (Level level) -> do
    case level of
      0 -> preOne (Score n)
      1 -> levelOne (Score n)
      2 -> preTwo   (Score n)
      3 -> levelTwo (Score n)
      4 -> preThree (Score n)
      5 -> levelThree (Score n)
 showFPS TimesRoman24 (250,0) 1.0 0.0 0.0
  

preOne :: GameAttribute -> PongAction()
preOne (Score n) = do
  ball <- findObject "ball" "ballGroup"
  bar <- findObject "bar" "barGroup"
  ball2 <- findObject "ball2" "ballGroup"
  ball3 <- findObject "ball3" "ballGroup"
  setObjectAsleep True ball2
  setObjectAsleep True ball3
  setGameState(GameInit 1)
  

levelOne :: GameAttribute -> PongAction()
levelOne (Score n) = do
 printOnScreen (show ("Press E to quit game")) Helvetica18 (65,275) 1.0 1.0 1.0
 stategame <- getGameState
 ball <- findObject "ball" "ballGroup"
 col1 <- objectLeftMapCollision ball
 col2 <- objectRightMapCollision ball
 when (col1 || col2) (reverseXSpeed ball)
 col3 <- objectTopMapCollision ball
 when col3 (reverseYSpeed ball)
 col4 <- objectBottomMapCollision ball
 when col4 (setGameState (GameInit 1))
 bar <- findObject "bar" "barGroup"
 col5 <- objectsCollision ball bar
 when col5
    (do 
       reverseYSpeed ball
       setGameAttribute (Score (n + 100))
    )

 when (n > 500) (setGameState (GameInit 2))
 

preTwo :: GameAttribute -> PongAction()
preTwo (Score n) = do
  ball2 <- findObject "ball2" "ballGroup"
  setObjectAsleep False ball2
  bar <- findObject "bar" "barGroup"
  setGameState(GameInit 3)


levelTwo :: GameAttribute -> PongAction()
levelTwo (Score n) = do
 ball2 <- findObject "ball2" "ballGroup"
 col1 <- objectLeftMapCollision ball2
 col2 <- objectRightMapCollision ball2
 when (col1 || col2) (reverseXSpeed ball2)
 col3 <- objectTopMapCollision ball2
 when col3 (reverseYSpeed ball2)
 col4 <- objectBottomMapCollision ball2
 when col4 (funExit)
 bar <- findObject "bar" "barGroup"
 col5 <- objectsCollision ball2 bar
 when col5
    (do 
       reverseYSpeed ball2
       setGameAttribute (Score (n + 100)))
 when (n > 1000) (setGameState (GameInit 4))    


preThree :: GameAttribute -> PongAction()
preThree (Score n) = do
  ball3 <- findObject "ball3" "ballGroup"
  setObjectAsleep False ball3
  bar <- findObject "bar" "barGroup"
  setGameState(GameInit 5)


levelThree :: GameAttribute -> PongAction()
levelThree (Score n) = do
 ball2 <- findObject "ball2" "ballGroup"
 setObjectAsleep True ball2
 ball3 <- findObject "ball3" "ballGroup"
 col1 <- objectLeftMapCollision ball3
 col2 <- objectRightMapCollision ball3
 when (col1 || col2) (reverseXSpeed ball3)
 col3 <- objectTopMapCollision ball3
 when col3 (reverseYSpeed ball3)
 col4 <- objectBottomMapCollision ball3
 when col4 (funExit)
 bar <- findObject "bar" "barGroup"
 col5 <- objectsCollision ball3 bar
 when col5
    (do 
       reverseYSpeed ball3
       setGameAttribute (Score (n + 100)))
 when (n > 500) (setGameState (GameInit 3))    

