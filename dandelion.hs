import Application
import GuiUtils

main :: IO ()
main = runGUI $ do
  app <- makeApplication
  return $ apWindow app
