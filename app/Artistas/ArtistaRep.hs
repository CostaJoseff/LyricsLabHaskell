module Artistas.ArtistaRep where

import qualified Data.ByteString.Lazy as BS
import Objetos.Artista
import Data.Aeson
import System.Directory

set:: [Artista] -> IO()
set artistas = do
  codificarArquivo artistas

codificarArquivo:: [Artista] -> IO()
codificarArquivo artistas = do
  dirAtual <- getCurrentDirectory
  BS.writeFile (dirAtual++"\\app\\artista.json") (encode artistas)

get:: IO (Maybe [Artista])
get = do
  dirAtual <- getCurrentDirectory
  json <- BS.readFile (dirAtual++"\\app\\artista.json")
  return (decode json)