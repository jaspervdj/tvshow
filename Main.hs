--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Monad                 (forever, void)
import qualified Data.Base64.Types             as Base64
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base64        as Base64
import qualified Data.ByteString.Builder       as Builder
import           Data.Char                     (toLower)
import           Data.IORef                    (IORef, atomicModifyIORef,
                                                newIORef, readIORef)
import           Data.List                     (sort)
import qualified Data.Vector                   as V
import qualified Network.HTTP.Types            as HTTP
import           Network.Socket                (PortNumber)
import           Network.Wai                   (Application, pathInfo,
                                                responseLBS, responseStream)
import qualified Network.Wai.Handler.Warp      as Warp
import           System.Directory              (listDirectory)
import           System.Environment            (getArgs, getProgName)
import           System.Exit                   (ExitCode (..), exitFailure)
import           System.FilePath               (takeBaseName, takeExtension,
                                                (</>))
import qualified System.IO                     as IO
import qualified System.Process                as Process
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

--------------------------------------------------------------------------------

data Config = Config
    { cFrameWidth      :: Int
    , cFrameHeight     :: Int
    , cFramesPerSecond :: Int
    , cPort            :: PortNumber
    , cCacheSize       :: Int
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { cFrameWidth      = 1920
    , cFrameHeight     = 1080
    , cFramesPerSecond = 10
    , cPort            = 8080
    , cCacheSize       = 10
    }

--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [imageDir] -> do
            images <- listImages imageDir
            if null images
                then error $ "No images found in " ++ imageDir
                else do
                    frame <- readFrame
                        (cFrameWidth config)
                        (cFrameHeight config)
                        (V.head images)
                    stateRef <- newIORef $ State 0 frame [(0, frame)]
                    let title = takeBaseName imageDir
                        env   = Env title images stateRef
                    IO.hPutStrLn IO.stderr $
                        "Listening on http://localhost:" <>
                        show (cPort config) <>
                        "/stream"
                    Warp.run (fromIntegral $ cPort config) (app config env)
        _ -> do
            progName <- getProgName
            IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " IMAGE_DIR"
            exitFailure
  where
    config = defaultConfig

--------------------------------------------------------------------------------

listImages :: FilePath -> IO (V.Vector FilePath)
listImages imageDir = do
    names <- listDirectory imageDir
    pure $ V.map (imageDir </>) $
        V.filter ((`elem` imageExts) . map toLower . takeExtension) $
        V.fromList $ sort names
  where
    imageExts = [".jpg", ".jpeg"]

--------------------------------------------------------------------------------

newtype Frame = Frame BS.ByteString

readFrame :: Int -> Int -> FilePath -> IO Frame
readFrame w h path = Process.withCreateProcess createProcess $
    \_ mbOut mbErr process -> do
        out <- maybe (fail "ffmpeg: no stdout handle") pure mbOut
        err <- maybe (fail "ffmpeg: no stderr handle") pure mbErr
        bytes <- BS.hGetContents out
        exitCode <- Process.waitForProcess process
        case exitCode of
            ExitSuccess   -> pure $ Frame bytes
            ExitFailure n -> do
                IO.hPutStrLn IO.stderr $ "ffmpeg: exit code " ++ show n
                errors <- BS.hGetContents err
                BS.hPutStr IO.stderr errors
                fail $ "Could not load frame " ++ path
  where
    dims = show w ++ ":" ++ show h
    vf = "scale=" ++ dims ++ ":force_original_aspect_ratio=decrease," ++
        "pad=" ++ dims ++ ":(ow-iw)/2:(oh-ih)/2"
    args =
        [ "-loglevel", "error"
        , "-i", path
        , "-vf", vf
        , "-frames:v", "1"
        , "-f", "image2pipe"
        , "-vcodec", "mjpeg"
        , "-"
        ]

    createProcess = (Process.proc "ffmpeg" args)
        { Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        }

--------------------------------------------------------------------------------

data Env = Env
    { eTitle :: String
    , eFiles :: V.Vector FilePath
    , eState :: IORef State
    }

data State = State
    { sIndex :: Int
    , sFrame :: Frame
    , sCache :: [(Int, Frame)]
    }

--------------------------------------------------------------------------------

app :: Config -> Env -> Application
app config env req respond
    | pathInfo req == ["stream"] = respond $ responseStream HTTP.status200
        [ (HTTP.hContentType, "multipart/x-mixed-replace; boundary=frame")
        , ("Cache-Control", "no-cache, no-store")
        ] $ \write flush -> forever $ do
            Frame jpeg <- fmap sFrame . readIORef $ eState env

            write $ Builder.byteString
                "--frame\r\nContent-Type: image/jpeg\r\n"

            write $ Builder.string8
                ("Content-Length: " ++ show (BS.length jpeg)
                    ++ "\r\n\r\n")

            write $ Builder.byteString jpeg
            write $ Builder.string8 "\r\n"
            flush

            threadDelay delay
    | pathInfo req == ["prev"] = do
        updateIndex prevIndex
        respond $ responseLBS HTTP.status303 [(HTTP.hLocation, "/")] ""
    | pathInfo req == ["next"] = do
        updateIndex nextIndex
        respond $ responseLBS HTTP.status303 [(HTTP.hLocation, "/")] ""
    | pathInfo req == [] = do
        state <- readIORef $ eState env
        let idx        = sIndex state
            Frame jpeg = sFrame state

            url = "data:image/jpeg;base64," <>
                H.toValue (Base64.extractBase64 $ Base64.encodeBase64 jpeg)
        respond $ responseLBS HTTP.status200
            [ (HTTP.hContentType, "text/html")
            ] $
            H.renderHtml $
            H.docTypeHtml $ do
                H.head $ do
                    H.meta H.! A.name "viewport"
                        H.! A.content "width=device-width, initial-scale=1"
                    H.style $ do
                        "body {"
                        "    display: grid;"
                        "    font-size: 2rem;"
                        "    text-align: center;"
                        "    margin: 0px;"
                        "    gap: 1em;"
                        "}"
                        "img {"
                        "    max-width: 100vw;"
                        "}"
                        "form {"
                        "    display: inline-block;"
                        "}"
                        "button {"
                        "    font-size: 3rem;"
                        "    width: 90vw;"
                        "}"
                H.body $ do
                    H.img H.! A.src url
                    H.form H.! A.method "POST" H.! A.action "/prev" $ do
                        H.button H.! A.type_ "submit" $ "Prev"
                    H.span $ do
                        H.toHtml $ eTitle env
                        H.br
                        H.toHtml (idx + 1) <> H.wbr <> "/"
                        H.toHtml (V.length (eFiles env))
                    H.form H.! A.method "POST" H.! A.action "/next" $ do
                        H.button H.! A.type_ "submit" $ "Next"
    | otherwise =
        respond $ responseLBS HTTP.status404
            [(HTTP.hContentType, "text/plain")]
            "Not found\n"
  where
    delay = (1000 * 1000) `div` cFramesPerSecond config

    nextIndex :: State -> Int
    nextIndex state
        | sIndex state + 1 < V.length (eFiles env) = sIndex state + 1
        | otherwise                                = 0

    prevIndex :: State -> Int
    prevIndex state
        | sIndex state <= 0 = V.length (eFiles env) - 1
        | otherwise         = sIndex state - 1

    updateIndex :: (State -> Int) -> IO ()
    updateIndex f = do
        oldState <- readIORef $ eState env
        let newIdx   = f oldState
        frame <- loadFrame newIdx
        newState <- atomicModifyIORef (eState env) $ \s ->
            let newState = s {sIndex = newIdx, sFrame = frame}
            in (newState, newState)
        void $ forkIO $ void $ loadFrame $ nextIndex newState

    loadFrame :: Int -> IO Frame
    loadFrame idx = do
        oldState <- readIORef $ eState env
        let file = eFiles env V.! idx
        frame <- case lookup idx (sCache oldState) of
            Just frame -> do
                IO.hPutStrLn IO.stderr $ "Cache hit: " ++ file
                pure frame
            Nothing -> do
                IO.hPutStrLn IO.stderr $ "Loading: " ++ file
                f <- readFrame (cFrameWidth config) (cFrameHeight config) file
                IO.hPutStrLn IO.stderr $ "Loaded: " ++ file
                pure f
        atomicModifyIORef (eState env) $ \s ->
            ( s { sCache = (:) (idx, frame) $
                    take (cCacheSize config - 1) $
                    filter ((/= idx) . fst) (sCache s)
                }
            , frame
            )
