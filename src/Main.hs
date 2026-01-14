module Main (main) where

import Data.Text.IO qualified as T
import Effectful (Eff, IOE, type (:>))
import Effectful qualified as E
import Effectful.Dispatch.Dynamic qualified as E
import Effectful.Error.Static qualified as E
import Effectful.Exception qualified as E
import Effectful.FileSystem.IO qualified as E
import Effectful.State.Static.Local qualified as E
import Effectful.TH qualified as E
import GHC.Float (double2Int, int2Double)
import ListT qualified

newtype ImageWidth = ImageWidth Int
newtype ImageHeight = ImageHeight Int
newtype ViewportWidth = ViewportWidth Double
newtype ViewportHeight = ViewportHeight Double
newtype ImagePixel = ImagePixel (Int, Int, Int)
newtype ImageHandleErr = ImageHandleErr Text deriving stock (Show)

data ImageHandle :: E.Effect where
  OpenImage :: FilePath -> ImageHandle m ()
  CloseImage :: ImageHandle m ()
  InitImage :: ImageWidth -> ImageHeight -> ImageHandle m ()
  PutPixelLn :: ImagePixel -> ImageHandle m ()

E.makeEffect ''ImageHandle

runImage
  :: (E.Error ImageHandleErr :> es, E.FileSystem :> es, IOE :> es)
  => Eff (ImageHandle : es) a
  -> Eff es a
runImage = E.reinterpret (E.evalState (Nothing :: Maybe Handle)) $ const $ \case
  OpenImage filePath -> do
    imageHandle <- E.openFile filePath WriteMode
    E.put (Just imageHandle)
  CloseImage -> do
    imageHandle <-
      E.get `whenNothingM` (E.throwError . ImageHandleErr $ "No image opened. Use `openImage` first.")
    E.hFlush imageHandle >> E.hClose imageHandle
  InitImage (ImageWidth imageWidth') (ImageHeight imageHeight') -> do
    imageHandle <-
      E.get `whenNothingM` (E.throwError . ImageHandleErr $ "No image opened. Use `openImage` first.")
    liftIO $ T.hPutStrLn imageHandle "P3"
    liftIO . T.hPutStrLn imageHandle $ show imageWidth' <> " " <> show imageHeight'
    liftIO $ T.hPutStrLn imageHandle "255"
  PutPixelLn (ImagePixel (r, g, b)) -> do
    imageHandle <-
      E.get `whenNothingM` (E.throwError . ImageHandleErr $ "No image opened. Use `openImage` first.")
    liftIO $ T.hPutStrLn imageHandle $ show r <> " " <> show g <> " " <> show b

data Logger :: E.Effect where
  Log :: Text -> Logger m ()
  LogLn :: Text -> Logger m ()

E.makeEffect ''Logger

runLogger :: IOE :> es => Eff (Logger : es) a -> Eff es a
runLogger = E.interpret $ const $ \case
  Log content -> liftIO $ putText content
  LogLn content -> liftIO $ putTextLn content

newtype Vec3 = Vec3 (Double, Double, Double)

instance Num Vec3 where
  (+) (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = Vec3 (x1 + x2, y1 + y2, z1 + z2)
  (-) (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = Vec3 (x1 - x2, y1 - y2, z1 - z2)
  (*) (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = Vec3 (x1 * x2, y1 * y2, z1 * z2)
  negate (Vec3 (x, y, z)) = Vec3 (negate x, negate y, negate z)
  abs (Vec3 (x, y, z)) = Vec3 (abs x, abs y, abs z)
  signum (Vec3 (x, y, z)) = Vec3 (signum x, signum y, signum z)
  fromInteger i = let iV = fromInteger i in Vec3 (iV, iV, iV)

class Vector3 v where
  xVal :: v -> Double
  yVal :: v -> Double
  zVal :: v -> Double
  scale :: v -> Double -> v
  vLength :: v -> Double
  lengthSquared :: v -> Double
  toText :: v -> Text
  dot :: v -> v -> Double
  cross :: v -> v -> v
  unitVector :: v -> v

instance Vector3 Vec3 where
  xVal (Vec3 (x, _, _)) = x
  yVal (Vec3 (_, y, _)) = y
  zVal (Vec3 (_, _, z)) = z
  scale vec k = vec * Vec3 (k, k, k)
  vLength vec = sqrt $ lengthSquared vec
  lengthSquared vec = vec `dot` vec
  toText (Vec3 (x, y, z)) = show x <> " " <> show y <> " " <> show z
  dot (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = x1 * x2 + y1 * y2 + z1 * z2
  cross (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = Vec3 (y1 * z2 - y2 * z1, z1 * x2 - z2 * x1, x1 * y2 - y1 * x2)
  unitVector vec = vec `scale` (1 / vLength vec)

newtype Color = Color (Double, Double, Double)
  deriving (Num, Vector3) via Vec3

pixel :: Color -> ImagePixel
pixel (Color (r, g, b)) =
  let !rByte = double2Int (255.999 * r)
      !gByte = double2Int (255.999 * g)
      !bByte = double2Int (255.999 * b)
  in ImagePixel (rByte, gByte, bByte)

newtype Point3 = Point3 (Double, Double, Double)
  deriving (Num, Vector3) via Vec3

data Ray = Ray {origin :: Point3, direction :: Vec3}

at :: Ray -> Double -> Point3
at ray t = ray.origin + coerce (ray.direction `scale` t)

rayColor :: Ray -> Color
rayColor ray =
  let unitDirection = unitVector ray.direction
      a = 0.5 * (yVal unitDirection + 1.0)
      center = Point3 (0.0, 0.0, -1.0)
      radius = 0.5
      t = hitSphere center radius ray
      n = unitVector $ ray `at` t - Point3 (0.0, 0.0, -1.0)
  in if t > 0.0 then
       Color (xVal n + 1, yVal n + 1, zVal n + 1) `scale` 0.5
     else
       Color (1.0, 1.0, 1.0) `scale` (1.0 - a) + Color (0.5, 0.7, 1.0) `scale` a

hitSphere :: Point3 -> Double -> Ray -> Double
hitSphere center radius ray =
  let oc = center - ray.origin
      a = lengthSquared ray.direction
      h = ray.direction `dot` coerce oc
      c = lengthSquared oc - radius * radius
      discriminant = h * h - a * c
  in if discriminant < 0.0 then
       -1.0
     else
       (h - sqrt discriminant) / a

type App =
  Eff
    '[ ImageHandle
     , Logger
     , E.FileSystem
     , E.Error ImageHandleErr
     , IOE
     ]

appRunner :: App a -> IO (Either ImageHandleErr a)
appRunner =
  E.runEff
    . E.runErrorNoCallStack
    . E.runFileSystem
    . runLogger
    . runImage

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageWidth :: ImageWidth
imageWidth = ImageWidth 400

imageHeight :: ImageHeight
imageHeight =
  ImageHeight
    $ max 1
    $ double2Int
    $ int2Double (coerce imageWidth) / aspectRatio

viewportHeight :: ViewportHeight
viewportHeight = ViewportHeight 2.0

viewportWidth :: ViewportWidth
viewportWidth =
  ViewportWidth
    $ coerce viewportHeight * (int2Double (coerce imageWidth) / int2Double (coerce imageHeight))

focalLength :: Double
focalLength = 1.0

cameraCenter :: Point3
cameraCenter = Point3 (0, 0, 0)

viewportU :: Vec3
viewportU = Vec3 (coerce viewportWidth, 0.0, 0.0)

viewportV :: Vec3
viewportV = Vec3 (0.0, -coerce viewportHeight, 0.0)

pixelDeltaU :: Vec3
pixelDeltaU = viewportU `scale` (1 / int2Double (coerce imageWidth))

pixelDeltaV :: Vec3
pixelDeltaV = viewportV `scale` (1 / int2Double (coerce imageHeight))

viewportUpperLeft :: Point3
viewportUpperLeft =
  cameraCenter
    - Point3 (0.0, 0.0, focalLength)
    - coerce viewportU `scale` 0.5
    - coerce viewportV `scale` 0.5

pixel00Loc :: Point3
pixel00Loc = viewportUpperLeft + coerce (pixelDeltaU + pixelDeltaV) `scale` 0.5

main :: IO ()
main = do
  result <- appRunner createImage
  case result of
    Left (ImageHandleErr err) -> putTextLn $ "Error: " <> err
    Right _ -> pass

createImage :: (ImageHandle :> es, Logger :> es) => Eff es ()
createImage =
  E.bracket_ (openImage "image.ppm") closeImage
    $ initImage imageWidth imageHeight >> printImagePixels

printImagePixels :: (ImageHandle :> es, Logger :> es) => Eff es ()
printImagePixels = withLog $ do
  j <- ListT.fromFoldable [0 .. coerce imageHeight - 1]
  lift . log $ "\rScanlines remaining: " <> show (coerce imageHeight - j) <> " "
  i <- ListT.fromFoldable [0 .. coerce imageWidth - 1]
  let pixelCenter =
        pixel00Loc
          + coerce (pixelDeltaU `scale` int2Double i)
          + coerce (pixelDeltaV `scale` int2Double j)
      rayDirection = coerce $ pixelCenter - cameraCenter
      ray = Ray cameraCenter rayDirection
      pixelColor = rayColor ray
  lift . putPixelLn . pixel $ pixelColor
 where
  withLog :: Logger :> es => ListT.ListT (Eff es) () -> Eff es ()
  withLog listT = E.finally (void $ ListT.toList listT) (logLn "\rDone.                 ")
