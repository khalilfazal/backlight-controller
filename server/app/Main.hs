{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent.Lock (Lock, new, with)
import Control.Monad           ((>=>), ap)
import Control.Monad.IfElse    ((>>=?))
import Control.Monad.Morph     (lift)
import Data.Functor            (($>))
import Data.Text.Lazy          (Text)
import System.FilePath         ((</>))
import Text.Printf             (printf)
import Web.Scotty              (post, scotty, param, parseParam)

main :: IO ()
main = do
    m <- maxBrightness
    lock <- new
    scotty 3000 $
        post "/:brightness" $
            param "brightness" >>= lift . (interpret >=> editWith lock . portionOf m)

maxBrightness :: IO Double
maxBrightness = (fmap read . readFile) fMax

interpret :: Text -> IO Double
interpret = ap (flip either return . returnError) parseParam

-- if error, return 2 which will cause nothing to happen (portionOf will return Nothing)
returnError :: Text -> Text -> IO Double
returnError e m = printf "%s" m >> putError e $> 2

putError :: Text -> IO ()
putError = printf ": \"%s\"\n"

-- value must be > 0 and <= max_brightness
portionOf :: RealFrac a => a -> a -> Maybe Integer
portionOf m x | 0 < x && x <= 1 = (Just . round . (* m)) x
              | otherwise     = Nothing

editWith :: Show a => Lock -> Maybe a -> IO ()
editWith lock = (>>=? (write lock . show))

write :: Lock -> String -> IO ()
write lock = with lock . writeFile fBrightness

base, fMax, fBrightness :: FilePath
--base        = "/sys/class/backlight/intel_backlight"
base        = "/sys/class/backlight/acpi_video0/"
fMax        = base </> "max_brightness"
fBrightness = base </> "brightness"
