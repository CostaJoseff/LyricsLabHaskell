module Musicas.MusicaRep where

import qualified Data.ByteString.Lazy as BS
import Objetos.Musica
import Data.Aeson

set :: [Musica] -> IO ()
set musicas = do
  codificarArquivoMusica musicas

codificarArquivoMusica :: [Musica] -> IO ()
codificarArquivoMusica musicas = BS.writeFile "../musica.json" (encode musicas)

get :: IO (Maybe [Musica])
get = do
  json <- BS.readFile "../musica.json"
  return (decode json)