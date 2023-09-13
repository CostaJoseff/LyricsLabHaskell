{-# LANGUAGE DeriveGeneric #-}
module Bandas.BandaRepository where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import Objetos.Banda



save :: [Banda] -> IO()
save banda = BS.writeFile "bandas.json" (encode banda)

save2 :: [Banda] -> IO()
save2 banda =

    BS.writeFile "bandas.json" (encode banda)



load :: IO (Maybe [Banda])
load = do
        json <- BS.readFile "bandas.json"
        return (decode json)



-- Funçoes de recuperação


