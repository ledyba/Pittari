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
import           Image ( loadImage, resize, DPI(DPI), Paper(Paper), ImageSize(ImageSize) )
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
                binary <- liftIO $ resize (DPI 300) (Paper 300.0 300.0) (ImageSize 300.0 300.0) img
                writeBS binary
        
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

