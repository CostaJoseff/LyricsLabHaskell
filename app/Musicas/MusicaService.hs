module Musicas.MusicaService where

import Musicas.MusicaRep as MRep
import Objetos.Musica
import Data.Time.Calendar
import Data.List (isInfixOf, isPrefixOf)

{-Função Pública-}
setMusica:: Musica -> IO()
setMusica musica = do
  maybeMusicas <- MRep.get
  case maybeMusicas of
    Just musicaLista -> MRep.set (musica:musicaLista)
    Nothing -> return ()

{-Função Pública-}
getMusicas :: IO [Musica]
getMusicas = do
  maybeMusicas <- MRep.get
  case maybeMusicas of
    Just musicasLista -> return musicasLista
    Nothing -> return []

{-Função Pública-}
getMusicaPorID :: String -> IO Musica
getMusicaPorID idAlvo = do
  musicas <- getMusicas
  case findMusicaByID idAlvo musicas of
    Just musicaEncontrada -> return musicaEncontrada
    Nothing -> return (Musica "NULL" [] [] "NULL" "NULL" "NULL" "NULL" 0)

findMusicaByID :: String -> [Musica] -> Maybe Musica
findMusicaByID _ [] = Nothing  -- Se a lista estiver vazia, retornar Nothing
findMusicaByID idAlvo (musica:resto)
  | idAlvo == idMusica musica = Just musica  -- Se o ID for encontrado, retornar Just com a música
  | otherwise = findMusicaByID idAlvo resto  -- Caso contrário, continuar a busca na lista restante




{-Função Pública-}
filtrarMusicasPorParticipante :: String -> IO [Musica]
filtrarMusicasPorParticipante nomeParticipante = do
  musicas <- getMusicas
  return (filter (\musica -> nomeParticipante `elem` participantes musica) musicas)

filterByParticipante :: String -> [Musica] -> [Musica] -> [Musica]
filterByParticipante _ [] result = result
filterByParticipante nomeParticipante (musica:musicasRestantes) result
  | nomeParticipante `elem` participantes musica = filterByParticipante nomeParticipante musicasRestantes (musica:result)
  | otherwise = filterByParticipante nomeParticipante musicasRestantes result

{-Função Pública-}
filtrarMusicasPorRitmo :: String -> IO [Musica]
filtrarMusicasPorRitmo nomeRitmo = do
  musicas <- getMusicas
  return (filterByRitmo nomeRitmo musicas [])

filterByRitmo :: String -> [Musica] -> [Musica] -> [Musica]
filterByRitmo _ [] result = result
filterByRitmo nomeRitmo (musica:musicasRestantes) result
  | nomeRitmo == ritmo musica = filterByRitmo nomeRitmo musicasRestantes (musica:result)
  | otherwise = filterByRitmo nomeRitmo musicasRestantes result
  


{-Função Pública-}
filtrarMusicasPorTrechoLetra :: String -> [Musica] -> [Musica]
filtrarMusicasPorTrechoLetra trecho musicas =
  filter (\musica -> trecho `isInfixOf` letra musica) musicas
  where
    isInfixOf :: Eq a => [a] -> [a] -> Bool
    isInfixOf [] _ = True
    isInfixOf _ [] = False
    isInfixOf needle haystack@(x:xs) =
      if isPrefixOf needle haystack
        then True
        else isInfixOf needle xs
