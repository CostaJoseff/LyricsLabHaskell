module Main (main) where

import Lib

import LyricsLibService as LLS
import Control.Exception
import Text.Read (readMaybe)
import Control.Concurrent (threadDelay)

main::IO()
main = do
  apresentacaoInicial
  resultado <- menuInicial
  if resultado == "0" then print ("Até mais!")
  else do
    case resultado of
      "11" -> do --Cadastrar Artista
        print ("###")
      "12" -> do --Cadastrar Banda
        print ("###")
      "13" -> do --Cadastrar Musica
        print ("###")
      "14" -> do --Adicionar Integrantes na Banda
        print ("###")
      "15" -> do --Remover Integrantes na Banda
        print ("###")
      "21" -> do --Buscar Artista
        putStrLn("Informe o nome do artista:")
        nome <- getLine
        resultado <- LLS.buscarArtistaPorNome nome
        putStrLn (show resultado)
        main
      "22" -> do --Buscar Banda
        putStrLn("Informe o nome da banda:")
        nome <- getLine
        resultado <- LLS.buscarBanda nome
        putStrLn (resultado)
        main
      "23" -> do --Buscar Musica
        putStrLn("Informe o nome da musica:")
        nome <- getLine
        resultado <- LLS.buscarMusica nome
        putStrLn (resultado)
        main
      "24" -> do --Filtrar Artistas
        menuFiltrarArtistas
        main
      "25" -> do --Filtrar Bandas
        menuFiltrarBandas
        main
      "26" -> do --Filtrar Musicas
        menuFiltrarMusicas
        main
      "27" -> do --Top Artistas
        putStrLn("Informe a quantidade de artistas que devem aparecer no TOP:")
        quantidade <- getLine
        let quantidadeInt = readMaybe quantidade :: Maybe Int
        case quantidadeInt of
          Just numero -> do
            artistasTop <- LLS.topArtistas numero
            putStrLn (show artistasTop)
          Nothing -> do
            putStrLn("\n\nOpção invalida, por favor digite uma opcao valida.\n\n")
            main
      "28" -> do --Top Bandas
        putStrLn("Informe a quantidade de bandas que devem aparecer no TOP:")
        quantidade <- getLine
        let quantidadeInt = readMaybe quantidade :: Maybe Int
        case quantidadeInt of
          Just numero -> do
            bandasTop <- LLS.topBandas numero
            putStrLn (show bandasTop)
          Nothing -> do
            putStrLn("\n\nOpção invalida, por favor digite uma opcao valida.\n\n")
            main
      "29" -> do --Top Musicas
        putStrLn("Informe a quantidade de musicas que devem aparecer no TOP:")
        quantidade <- getLine
        let quantidadeInt = readMaybe quantidade :: Maybe Int
        case quantidadeInt of
          Just numero -> do
            musicasTop <- LLS.topMusicas numero
            putStrLn (show musicasTop)
          Nothing -> do
            putStrLn("\n\nOpção invalida, por favor digite uma opcao valida.\n\n")
            main
      

apresentacaoInicial:: IO()
apresentacaoInicial = do
  print("Bem vindo ao Lyrics-LIB")
  threadDelay (2 * 1000000)
  putStrLn $ replicate 50 '\n'

menuInicial:: IO (String)
menuInicial = do
  putStrLn("----------")
  putStrLn("1 - Cadastros")
  putStrLn("2 - Buscas")
  putStrLn("3 - DashBoard")
  putStrLn("0 - Sair")
  putStrLn("----------\n\n")
  entrada <- getLine
  let entradaInt = readMaybe entrada :: Maybe Int
  case entradaInt of
    Just numero -> do
      case numero of
        1 -> do
          menuCadastros "1"
        2 -> do
          menuBuscas "2"
        3 -> do
          menuDashBoard
        0 -> return ("0")
    Nothing -> do
      putStrLn("\n\nOpção invalida, por favor digite uma opcao valida.\n\n")
      menuInicial

menuCadastros:: String -> IO (String)
menuCadastros num1 = do
  putStrLn("----------")
  putStrLn("1 - Cadastrar Artista")
  putStrLn("2 - Cadastrar Banda")
  putStrLn("3 - Cadastrar Musica")
  putStrLn("----------")
  putStrLn("4 - Adicionar Integrantes na Banda")
  putStrLn("5 - Remover Integrantes na Banda")
  putStrLn("----------\n\n")
  return ("###")

menuBuscas:: String -> IO (String)
menuBuscas num1 = do
  putStrLn("----------")
  putStrLn("1 - Buscar Artista")
  putStrLn("2 - Buscar Banda")
  putStrLn("3 - Buscar Musica")
  putStrLn("----------")
  putStrLn("4 - Filtrar artistas")
  putStrLn("5 - Filtrar Bandas")
  putStrLn("6 - Filtrar Musicas")
  putStrLn("----------")
  putStrLn("7 - Top Artistas")
  putStrLn("8 - Top Bandas")
  putStrLn("9 - Top Musicas")
  putStrLn("----------\n\n")
  entrada <- getLine
  let entradaConvertida = readMaybe entrada :: Maybe Int
  case entradaConvertida of
    Just numero -> do
      case numero of
        1 -> return (num1++"1")
        2 -> return (num1++"2")
        3 -> return (num1++"3")
        4 -> return (num1++"4")
        5 -> return (num1++"5")
        6 -> return (num1++"6")
        7 -> return (num1++"7")
        8 -> return (num1++"8")
        9 -> return (num1++"9")

    Nothing -> do
      putStrLn("\n\nOpção invalida, por favor digite uma opcao valida.\n\n")
      menuBuscas num1


menuDashBoard:: IO (String)
menuDashBoard = return ("Em Breve!")

menuFiltrarArtistas:: IO()
menuFiltrarArtistas = do
  putStrLn("-----Filtrar Artistas-----")
  putStrLn("Opcao unica - filtrar artista por funcao")
  putStrLn("\nIndique a funcao a ser filtrada:")
  funcao <- getLine
  resultado <- LLS.filtrarArtistasPorFuncao funcao
  putStrLn (show resultado)

menuFiltrarBandas:: IO()
menuFiltrarBandas = do
  putStrLn("-----Filtrar Bandas-----")
  putStrLn("1 - Por genero")
  putStrLn("2 - Por instrumento")
  entrada <- getLine
  let entradaInt = readMaybe entrada :: Maybe Int
  case entradaInt of
    Just numero -> do
      case numero of
        1 -> do
          putStrLn("Indique o genero a ser filtrado:")
          genero <- getLine
          resultado <- LLS.filtrarBandasPorGenero genero
          putStrLn (show resultado)
        2 -> do
          putStrLn("Indique o instrumento a ser filtrado:")
          genero <- getLine
          resultado <- LLS.filtrarBandasPorInstrumento genero
          putStrLn (show resultado)
    Nothing -> do
      putStrLn("\n\nOpção invalida, por favor digite uma opcao valida.\n\n")
      menuFiltrarBandas

menuFiltrarMusicas:: IO ()
menuFiltrarMusicas = do
  putStrLn("-----Filtrar Musicas-----")
  putStrLn("1 - Por ritmo")
  putStrLn("2 - Por instrumento")
  putStrLn("3 - Por trecho")
  entrada <- getLine
  let entradaInt = readMaybe entrada :: Maybe Int
  case entradaInt of
    Just numero -> do
      case numero of
        1 -> do
          putStrLn("Indique o ritmo a ser filtrado:")
          ritmo <-getLine
          resultado <- LLS.filtrarMusicasPorRitmo ritmo
          putStrLn (show resultado)
        2 -> do
          putStrLn("Indique o instrumento a ser filtrado:")
          instrumento <-getLine
          resultado <- LLS.filtrarMusicasPorInstrumento instrumento
          putStrLn (show resultado)
        3 -> do
          putStrLn("Indique o trecho a ser filtrado:")
          trecho <-getLine
          resultado <- LLS.filtrarMusicasPorTrecho trecho
          putStrLn (show resultado)

    Nothing -> do
      putStrLn("\n\nOpção invalida, por favor digite uma opcao valida.\n\n")
      menuFiltrarMusicas