module WebServer where

import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock
import Happstack.Server
import Happstack.Helpers
import Text.StringTemplate
import Text.StringTemplate.Helpers

main :: IO ()
main = do putStrLn "Lambdasim now listening on port 8080"
          simpleHTTP (Conf 8080 Nothing) mainHandler

mainHandler :: ServerPartT IO Response
mainHandler =
  exactdir "/" $
    liftIO $ do
      time <- getCurrentTime
      templates <- directoryGroup "templates"
      let r = renderTemplateGroup templates [("time", time)] "index"
      return . toResponse . HtmlString $ r

emptyAttrs :: [(String, String)]
emptyAttrs = []
