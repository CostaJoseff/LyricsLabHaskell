module LyricsLibService where

import Control.Exception
import System.IO.Error

import Artistas.ArtistaService as AServ
import Objetos.Artista

import Bandas.BandaService
import Musicas.MusicaService


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

topMusicas:: Int -> IO [String]
topMusicas xMelhores = return [("")]

-- Funcoes Banda

buscarBanda:: String -> IO String
buscarBanda nomeBanda = return ("")

topBandas:: Int -> IO [String]
topBandas xMelhores = return [("")]

filtrarBandasPorInstrumento:: String -> IO [String]
filtrarBandasPorInstrumento instrumento = return [("")]

filtrarBandasPorGenero:: String -> IO [String]
filtrarBandasPorGenero genero = return [("")]

--Funcoes Musica

getMusicaPorNome :: String -> IO String
getMusicaPorNome nomeMusica = return ("")

filtrarMusicasPorTrechoLetra:: String -> IO [String]
filtrarMusicasPorTrechoLetra trecho = return [("")]


filtrarMusicasPorInstrumento:: String -> IO [String]
filtrarMusicasPorInstrumento instrumento = return [("")]

filtrarMusicasPorRitmo:: String -> IO [String]
filtrarMusicasPorRitmo ritmo = return [("")]

filtrarMusicasPorParticipante :: String -> IO [String]
filtrarMusicasPorParticipante participante = return [("")]
