{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent.Lock      (Lock, new, with)
import           Control.Monad                ((>=>))
import           Control.Monad.Morph          (lift)
import qualified Data.Text.Lazy          as T (Text, append)
import qualified Data.Text.Lazy.IO       as T (putStr, putStrLn)
import           System.FilePath              ((</>))
import           Web.Scotty                   (post, scotty, param, parseParam)

main :: IO ()
main = do
    m <- fmap fromIntegral maxBrightness
    lock <- new
    scotty 3000 $
        post "/:brightness" $
            param "brightness" >>= lift . (interpret >=> change lock . portionOf m)

maxBrightness :: IO Int
maxBrightness = (fmap read . readFile) fMax

interpret :: T.Text -> IO Double
interpret = flip either return . returnError >>= (. parseParam)

returnError :: T.Text -> T.Text -> IO Double
returnError e = (>> return 2) . (>> putError e) . T.putStr

putError :: T.Text -> IO ()
putError = T.putStrLn . (": \"" `T.append`) . (`T.append` "\"")

portionOf :: RealFrac a => a -> a -> Maybe Integer
portionOf m x | x >= 0 && x <= 1 = (Just . round . (* m)) x
              | otherwise        = Nothing

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
