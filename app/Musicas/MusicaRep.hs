module Musicas.MusicaRep where

import qualified Data.ByteString.Lazy as BS
import Objetos.Musica
import Data.Aeson
import System.Directory
import Data.Maybe (mapMaybe)

set :: [Musica] -> IO ()
set musicas = do
  codificarArquivo musicas

codificarArquivo:: [Musica] -> IO()
codificarArquivo musica = do
  dirAtual <- getCurrentDirectory
  BS.writeFile (dirAtual++"\\app\\musica.json") (encode musica)

get:: IO (Maybe [Musica])
get = do
  dirAtual <- getCurrentDirectory
  json <- BS.readFile (dirAtual++"\\app\\musica.json")
  return (decode json)


      
      

