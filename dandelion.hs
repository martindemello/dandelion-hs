import Application
import GuiUtils
import Types

main :: IO ()
main = runGUI $ do
  app <- makeApplication
  return $ apWindow app
