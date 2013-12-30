{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent.Lock (Lock, new, with)
import Control.Error.Safe      (rightZ)
import Control.Monad.Morph     (lift)
import Data.Text.Lazy          (Text)
import System.FilePath         ((</>))
import Web.Scotty              (post, scotty, param, parseParam)

main :: IO ()
main = do
    m <- fmap fromIntegral maxBrightness
    lock <- new
    scotty 3000 $
        post "/:brightness" $
            param "brightness" >>= lift . change lock . portionOf m . interpret

maxBrightness :: IO Int
maxBrightness = (fmap read . readFile) fMax

interpret :: Text -> Double
interpret = head . rightZ . parseParam

change :: Show a => Lock -> Maybe a -> IO ()
change = maybeM . (. show) . write

maybeM :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeM = (maybe . return) ()

write :: Lock -> String -> IO ()
write = (. writeFile fBrightness) . with

base, fMax, fBrightness :: FilePath
base        = "/sys/class/backlight/intel_backlight"
fMax        = base </> "max_brightness"
fBrightness = base </> "brightness"

portionOf :: RealFrac a => a -> a -> Maybe Integer
portionOf m x | x >= 0 && x <= 1 = (Just . round . (* m)) x
              | otherwise        = Nothing
