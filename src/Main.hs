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

newtype ImageWidth = ImageWidth Int
newtype ImageHeight = ImageHeight Int
newtype ColorR = ColorR Int
newtype ColorG = ColorG Int
newtype ColorB = ColorB Int
newtype ImagePixel = ImagePixel (ColorR, ColorG, ColorB)
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
  PutPixelLn (ImagePixel (ColorR r, ColorG g, ColorB b)) -> do
    imageHandle <- E.get `whenNothingM` (E.throwError . ImageHandleErr $ "No image opened. Use `openImage` first.")
    liftIO $ T.hPutStrLn imageHandle $ show r <> " " <> show g <> " " <> show b

type App = Eff '[Image, E.FileSystem, E.Error ImageHandleErr, IOE]

appRunner :: App a -> IO (Either ImageHandleErr a)
appRunner =
  E.runEff
    . E.runErrorNoCallStack
    . E.runFileSystem
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

createImage :: (Image :> es) => Eff es ()
createImage = E.bracket_ (openImage "image.ppm") closeImage $ do
  initImage imageWidth imageHeight
  mapM_ putPixelLn imagePixels

imagePixels :: [ImagePixel]
imagePixels = do
  let ImageHeight imageHeight' = imageHeight
      ImageWidth imageWidth' = imageWidth
  j <- [0 .. imageHeight' - 1]
  i <- [0 .. imageWidth' - 1]
  let !r = double2Int (255.999 * (int2Double i / int2Double (imageWidth' - 1)))
      !g = double2Int (255.999 * (int2Double j / int2Double (imageHeight' - 1)))
      !b = double2Int (255.999 * 0.0)
  pure $ ImagePixel (ColorR r, ColorG g, ColorB b)
