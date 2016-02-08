{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ZoomHub.API
  ( app
  ) where

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Either       (EitherT, left)
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy.Char8       as BLC
import           Data.Monoid                      ((<>))
import           Data.Proxy                       (Proxy (Proxy))
import           Network.Wai                      (Application)
import           Servant                          ((:<|>) (..), (:>), Capture,
                                                   Get, JSON, QueryParam,
                                                   ServantErr, Server, err301,
                                                   err400, err404, errBody,
                                                   errHeaders, serve)

import           ZoomHub.Config                   (Config)
import qualified ZoomHub.Config                   as Config
import           ZoomHub.Storage.File             (create, getById, getByURL)
import           ZoomHub.Types.Content            (Content, fromInternal)
import qualified ZoomHub.Types.Internal.Content   as Internal
import           ZoomHub.Types.Internal.ContentId (ContentId, unId)


-- Servant default handler type
type Handler a = EitherT ServantErr IO a

-- API
type API =
  -- TODO: Figure out how to route to `/`. Apparently `""` nor `"/"` works
  -- despite a hint here: https://git.io/vzEZx
       "welcome" :> Get '[JSON] String
  :<|> "v1" :> "content" :> Capture "id" ContentId :> Get '[JSON] Content
  :<|> "v1" :> "content" :> QueryParam "url" String :> Get '[JSON] Content

-- Handlers
welcome :: Handler String
welcome = return "Welcome to ZoomHub."

contentById :: String -> ContentId -> Handler Content
contentById dataPath contentId = do
  maybeContent <- liftIO $ getById dataPath contentId
  case maybeContent of
    Nothing      -> left err404{ errBody = error404message }
    Just content -> return $ fromInternal content
  where error404message = "No content with ID: " <> (BLC.pack $ unId contentId)

contentByURL :: Config -> Maybe String -> Handler Content
contentByURL config maybeURL = case maybeURL of
  Nothing  -> left err400{ errBody = "Missing ID or URL." }
  Just url -> do
      maybeContent <- liftIO $ getByURL (Config.dataPath config) url
      content <- case maybeContent of
        Nothing -> liftIO $ create config url
        Just c  -> return c
      redirect $ Internal.contentId content
      where
        -- NOTE: Enable Chrome developer console ‘[x] Disable cache’ to test
        -- permanent HTTP 301 redirects:
        redirect contentId =
          let location = BC.pack $ "/v1/content/" ++ unId contentId in
          left $ err301{
            -- HACK: Redirect using error: http://git.io/vBCz9
            errHeaders = [("Location", location)]
          }

-- API
api :: Proxy API
api = Proxy

server :: Config -> Server API
server config = welcome
           :<|> contentById (Config.dataPath config)
           :<|> contentByURL config

app :: Config -> Application
app config = serve api (server config)