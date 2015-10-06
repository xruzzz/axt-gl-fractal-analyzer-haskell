{-# LANGUAGE UnicodeSyntax #-}
module Main where
-- https://wiki.haskell.org/OpenGLTutorial2
import System.Environment
--import System.Directory
import System.IO
import Data.List
import Data.Char
import Data.IORef
import Control.Monad
-- import Prelude.Unicode
import Data.Time.Clock.POSIX
-- import Fractals.Tree
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT
import GL.Types(V2FL, V3FL, Coor2(..), Coor3(..), fromDegrees)

data State = State {
               deltaX      :: GLdouble
             , deltaY      :: GLdouble
             , moveSpeed   :: GLdouble
             , angle       :: GLdouble
             , rotateSpeed :: GLdouble
             , tstamp      :: GLdouble
             }

main :: IO ()
main = do
    let width = 1280
    let height = 1024
    let orthoWidth = 40
    let orthoHeight = 30
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    initialWindowSize $= Size width height
    createWindow "AXT GL Fractals - www.axi.su - xruzzzz@gmail.com"
    now <- getTimestamp
{-    state <- newIORef State { deltaX = 0, deltaY = 0, moveSpeed = 0.5
                          , angle = 0.0, rotateSpeed = 0.2
                          , tstamp = now
                          }
    depthFunc $= Just Less -- the comparison function for depth the buffer-}
    angle <- newIORef $ pi/2
    delta <- newIORef $ pi/360
    pos <- newIORef (0, 0)
    keyboardMouseCallback $= Just (keyboardMouse delta pos)
    reshapeCallback $= Just reshape
    idleCallback $= Just (idle angle delta)
    displayCallback $= display2 (D3  0 (-1.5) 0) angle
--    matrixMode $= Projection
    loadIdentity
--    ortho2D 0 40 0 30
--    matrixMode $= Modelview 0
    
    mainLoop

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing

getTimestamp :: IO GLdouble
getTimestamp = do
  now <- getPOSIXTime
  return $ fromRational $ toRational now

display2 :: Coor3->IORef GLfloat->DisplayCallback
display2 c ang= do
    clear [ColorBuffer, DepthBuffer]
    a <- get ang
    preservingMatrix $ do
        scale 0.5 0.5 (0.5::GLfloat)
        color $ Color3 (0.2::GLfloat) 0.8 0.8
        renderPrimitive Lines $ do
            treeT3D 15 c (pi/2) a
    swapBuffers
 
corner r g b x y = do color  (Color3  r g b :: Color3  GLfloat)
                      vertex (Vertex2 x y   :: V2FL)
{- 
  clear [ColorBuffer]
  renderPrimitive Lines $ do
    vertex (Vertex2 5 13   :: V2FL)
    vertex (Vertex2 16 2   :: V2FL)
  renderPrimitive Points $ do
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
    vertex (Vertex2 4 15   :: V2FL)
  renderPrimitive Triangles $ do
    corner 1 0 0 5 5
    corner 0 1 0 15 5
    corner 0 0 1 5 12
  renderPrimitive Polygon $
    mapM_ (\(x, y, z)->vertex$ Vertex3 x y z) polyPoints
  swapBuffers-}
corner r g b x y = do color  (Color3  r g b :: Color3  GLfloat)
                      vertex (Vertex2 x y   :: V2FL)

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse a p key Down _ _ = case key of
  (Char ' ') -> a $~! negate
  (Char '+') -> a $~! (* 2)
  (Char '-') -> a $~! (/ 2)
  (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyDown ) -> p $~! \(x,y) -> (x,y-0.1)
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()

rotateXZ an (x, y, z) =
  let x' = (x * cos an) - (z * sin an)
      z' = (x * sin an) + (z * cos an)
   in (x', y, z')

moveXY dx dy (x, y, z) =
  (x + dx, y + dy, z)

drawTriangle clr p1 p2 p3 = do
  let (p1x, p1y, p1z) = p1
      (p2x, p2y, p2z) = p2
      (p3x, p3y, p3z) = p3
  currentColor $= clr
  renderPrimitive Triangles $ do
    vertex $ Vertex3 p1x p1y p1z
    vertex $ Vertex3 p2x p2y p2z
    vertex $ Vertex3 p3x p3y p3z

drawField = do
    color $ Color3 (0::GLfloat) 0.7 0.8
    renderPrimitive Lines $ do
        vertex (Vertex3 0 0 0  :: Vertex3 GLfloat)
        vertex (Vertex3 5 0 5 :: Vertex3 GLfloat)

fTree::GLfloat -> IO ()
fTree 0 =do
    color $ Color3 (0::GLfloat) 0.7 0.5
    renderPrimitive Lines $ do
        vertex (Vertex2 0 0 :: V2FL)
        vertex (Vertex2 1 5 :: V2FL)

fTree n = do
    color $ Color3 (0::GLfloat) 0.2 0.8
    renderPrimitive Lines $ do
        vertex (Vertex2 (20*(cos n)) 5 :: V2FL)
        vertex (Vertex2 (20*(sin n)) (20*(cos n)) :: V2FL)
    fTree (n-1)

treeT::GLfloat-> Coor2->GLfloat->GLfloat->IO ()
treeT n co1 ang ang2
    | n > 0.5 = do
            let cEnd = getEnd (n/6) co1 ang
            putStrLn $ show co1 ++ show cEnd ++ (show $ cos ang)
            line co1 cEnd
            treeT (5*n/7) cEnd (ang + ang2) ang2
            treeT (5*n/7) cEnd (ang - ang2) ang2
    | otherwise = putStr ""

getEnd::GLfloat->Coor2->GLfloat->Coor2
getEnd l c ang = D2 (x c + l * cos ang) (y c + l * sin ang)

line::Coor2 -> Coor2 -> IO ()
line beg end = do
        vertex (Vertex2 (x beg) (y beg))
        vertex (Vertex2 (x end) (y end))

line3D::Coor3 -> Coor3 -> IO ()
line3D beg end = do
        vertex (Vertex3 (xx beg) (yy beg) (zz beg))
        vertex (Vertex3 (xx end) (yy end) (zz end))

treeT3D::GLfloat-> Coor3->GLfloat->GLfloat->IO ()
treeT3D n co1 ang ang2
    | n > 0.1 = do
            let cEnd = getEnd3D (n/20) co1 ang
            line3D co1 cEnd
            treeT3D (9*n/17) cEnd (ang + ang2) ang2
            treeT3D (15*n/17) cEnd (ang - ang2) ang2
    | otherwise = putStr ""

getEnd3D::GLfloat->Coor3->GLfloat->Coor3
getEnd3D l c ang = D3 (xx c + l * cos ang) (yy c + l * sin ang) 0
