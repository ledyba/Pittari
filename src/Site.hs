{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import qualified Heist.Interpreted as I
import           Snap.Util.FileUploads
import           Snap.Iteratee
import           Control.Monad.Trans
import           System.IO.Temp
import qualified Data.ByteString.Char8 as C
import           Image ( flipPaper, loadImage, resize, Paper(Paper), ImageSize(ImageSize) )
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

a1 = Paper 59.4 84.1
a2 = Paper 42.0 59.4
a3 = Paper 29.7 42.0
a4 = Paper 21.0 29.7
a5 = Paper 14.8 21.0
a6 = Paper 10.5 14.8

b1 = Paper 72.8 103.0
b2 = Paper 51.5 72.8
b3 = Paper 36.4 51.5
b4 = Paper 25.7 36.4
b5 = Paper 18.2 25.7
b6 = Paper 12.8 18.2


decodePaper p = case p of
                    Nothing -> a4
                    Just x -> case x of
                                 "A1" -> a1
                                 "A2" -> a2
                                 "A3" -> a3
                                 "A4" -> a4
                                 _ -> a4

handleUpload :: Handler App App ()
handleUpload = method GET (redirect "/") <|> method POST handlePost
  where
    handlePost = liftSnap $ handleFileUploads "/tmp/" defaultUploadPolicy (const (allowWithMaximumSize (1024*1024*10))) out
    out :: [(PartInfo, Either PolicyViolationException FilePath)] -> Snap ()
    out files = do
        (_, Right f):_ <- return files
        outPDF f
    outPDF :: FilePath -> Snap ()
    outPDF file = do
        eimg <- liftIO $ loadImage file
        case eimg of
            Left x -> writeText $ T.pack x
            Right img -> do
                imageWidth_ <- getParam "imageWidth"
                imageHeight_ <- getParam "imageHeight"
                imageWidth <- return $ case imageWidth_ of
                                        Just x -> (read (C.unpack x)) :: Float
                                        Nothing -> 10.0
                imageHeight <- return $ case imageHeight_ of
                                        Just x -> (read (C.unpack x)) :: Float
                                        Nothing -> 10.0
                paper <- getParam "paper"
                paperSize <- return $ (decodePaper paper)
                binary <- liftIO $ resize (if imageWidth > imageHeight then flipPaper paperSize else paperSize) (ImageSize imageWidth imageHeight) img
                writeBS binary
                where
                  
        
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",          serveDirectory "static")
          ,("/upload",   handleUpload)
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes routes
    return $ App h s

