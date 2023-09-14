module LyricsLibService where

import Control.Exception
import System.IO.Error
import Text.Read (readMaybe)

import Artistas.ArtistaService as AServ
import Objetos.Artista

import Bandas.BandaService


-- Funcoes Artistas

buscarArtistaPorID:: String -> IO String
buscarArtistaPorID idAlvo = do
  artista <- AServ.getArtista idAlvo
  if nome artista == "NULL" then return "Artista nao encontrado."
  else return (show artista)

buscarArtistaPorNome:: String -> IO [String]
buscarArtistaPorNome nomeAlvo = do
  resultado <- AServ.filtrarArtistasPorNome nomeAlvo
  return (map (\artista -> (show artista)) resultado)

filtrarArtistasPorFuncao:: String -> IO [String]
filtrarArtistasPorFuncao funcao = do
  resultado <- AServ.filtrarArtistasPorFuncao funcao
  return (map (\artista -> (show artista)) resultado)

topArtistas:: Int -> IO [String]
topArtistas xMelhores = return [("")]

cadastrarArtista:: [String] -> [String] -> IO ()
cadastrarArtista dados bandas = do
  numero <- AServ.len
  let novoArtista = Artista (show(numero+1)) (dados!!0) (dados!!1) bandas (dados!!2)
  AServ.setArtista novoArtista

-- Funcoes Banda

buscarBanda:: String -> IO String
buscarBanda nomeBanda = return ("")

topBandas:: Int -> IO [String]
topBandas xMelhores = return [("")]

filtrarBandasPorInstrumento:: String -> IO [String]
filtrarBandasPorInstrumento instrumento = return [("")]

filtrarBandasPorGenero:: String -> IO [String]
filtrarBandasPorGenero genero = return [("")]

cadastrarBanda:: [String] -> [String] -> [String] -> [String] -> [String] -> IO ()
cadastrarBanda dados artistasAtuais artistasAnteriores musicas instrumentos

--Funcoes Musica

buscarMusica:: String -> IO String
buscarMusica nomeMusica = return ("")

filtrarMusicasPorTrecho:: String -> IO [String]
filtrarMusicasPorTrecho trecho = return [("")]

filtrarMusicasPorInstrumento:: String -> IO [String]
filtrarMusicasPorInstrumento instrumento = return [("")]

filtrarMusicasPorRitmo:: String -> IO [String]
filtrarMusicasPorRitmo ritmo = return [("")]

topMusicas:: Int -> IO [String]
topMusicas xMelhores = return [("")]