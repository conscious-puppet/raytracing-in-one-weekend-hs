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
newtype ImagePixel = ImagePixel (Int, Int, Int)
newtype ImageHandleErr = ImageHandleErr Text deriving stock (Show)

data Image :: E.Effect where
  OpenImage :: FilePath -> Image m ()
  CloseImage :: Image m ()
  InitImage :: ImageWidth -> ImageHeight -> Image m ()
  PutPixelLn :: ImagePixel -> Image m ()

E.makeEffect ''Image

runImage ::
  (IOE :> es, E.Error ImageHandleErr :> es, E.FileSystem :> es) =>
  Eff (Image : es) a ->
  Eff es a
runImage = E.reinterpret (E.evalState (Nothing :: Maybe Handle)) $ const $ \case
  OpenImage filePath -> do
    imageHandle <- E.openFile filePath WriteMode
    E.put (Just imageHandle)
  CloseImage -> do
    imageHandle <- E.get `whenNothingM` (E.throwError . ImageHandleErr $ "No image opened. Use `openImage` first.")
    E.hFlush imageHandle >> E.hClose imageHandle
  InitImage (ImageWidth imageWidth') (ImageHeight imageHeight') -> do
    imageHandle <- E.get `whenNothingM` (E.throwError . ImageHandleErr $ "No image opened. Use `openImage` first.")
    liftIO $ T.hPutStrLn imageHandle "P3"
    liftIO . T.hPutStrLn imageHandle $ show imageWidth' <> " " <> show imageHeight'
    liftIO $ T.hPutStrLn imageHandle "255"
  PutPixelLn (ImagePixel (r, g, b)) -> do
    imageHandle <- E.get `whenNothingM` (E.throwError . ImageHandleErr $ "No image opened. Use `openImage` first.")
    liftIO $ T.hPutStrLn imageHandle $ show r <> " " <> show g <> " " <> show b

data Logger :: E.Effect where
  Log :: Text -> Logger m ()
  LogLn :: Text -> Logger m ()

E.makeEffect ''Logger

runLogger ::
  (IOE :> es) =>
  Eff (Logger : es) a ->
  Eff es a
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
  fromInteger i = let iD = fromInteger i in Vec3 (iD, iD, iD)

class Vector3 v where
  xVal :: v -> Double
  yVal :: v -> Double
  zVal :: v -> Double
  scale :: Double -> v -> v
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
  scale k vec = Vec3 (k, k, k) * vec
  vLength vec = sqrt $ lengthSquared vec
  lengthSquared vec = dot vec vec
  toText (Vec3 (x, y, z)) = show x <> " " <> show y <> " " <> show z
  dot (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = x1 * x2 + y1 * y2 + z1 * z2
  cross (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = Vec3 (y1 * z2 - y2 * z1, z1 * x2 - z2 * x1, x1 * y2 - y1 * x2)
  unitVector vec = scale (1 / vLength vec) vec

newtype Color = Color Vec3
  deriving (Num) via Vec3
  deriving (Vector3) via Vec3

class ColorRGB v where
  pixel :: v -> ImagePixel

instance ColorRGB Color where
  pixel (Color (Vec3 (r, g, b))) =
    let !rByte = double2Int (255.999 * r)
        !gByte = double2Int (255.999 * g)
        !bByte = double2Int (255.999 * b)
     in ImagePixel (rByte, gByte, bByte)

newtype Point3 = Point Vec3
  deriving (Num) via Vec3
  deriving (Vector3) via Vec3

type App =
  Eff
    '[ Image
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

imageWidth :: ImageWidth
imageWidth = ImageWidth 256

imageHeight :: ImageHeight
imageHeight = ImageHeight 256

main :: IO ()
main = do
  result <- appRunner createImage
  case result of
    Left (ImageHandleErr err) -> putTextLn $ "Error: " <> err
    Right _ -> pass

createImage :: (Logger :> es, Image :> es) => Eff es ()
createImage = E.bracket_ (openImage "image.ppm") closeImage $ do
  initImage imageWidth imageHeight >> printImagePixels

printImagePixels :: (Image :> es, Logger :> es) => Eff es ()
printImagePixels = withLog $ do
  let ImageHeight imageHeight' = imageHeight
      ImageWidth imageWidth' = imageWidth
  j <- ListT.fromFoldable [0 .. imageHeight' - 1]
  lift . log $ "\rScanlines remaining: " <> show (imageHeight' - j) <> " "
  i <- ListT.fromFoldable [0 .. imageWidth' - 1]
  let !r = int2Double i / int2Double (imageWidth' - 1)
      !g = int2Double j / int2Double (imageHeight' - 1)
      !b = 0.0
      color = Color (Vec3 (r, g, b))
  lift . putPixelLn . pixel $ color
  where
    withLog :: (Logger :> es) => ListT.ListT (Eff es) () -> Eff es ()
    withLog listT = E.finally (void $ ListT.toList listT) (logLn "\rDone.                 ")
